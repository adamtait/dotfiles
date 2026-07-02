# Provider pitfalls: stateful resources & force-replacement attributes

The danger in a plan is rarely the additions — it's **replacements** (a resource
destroyed and recreated because an immutable attribute changed) and **destroys**
of things that hold state. This catalog is the reference for "is this resource
stateful?" and "does changing this attribute force a replace?". Don't recall
these from memory; the immutability rules are provider-specific and easy to get
wrong.

How to use: when the change set shows a `replace` (`["delete","create"]`), find
the resource type below and check whether the `replace_paths` attribute is a
known force-new field, and whether the resource holds data. A replace of a
stateful resource is almost always at least High, usually Critical.

## Contents
- [How replacement shows up](#how-replacement-shows-up)
- [GCP (google / google-beta)](#gcp)
- [AWS](#aws)
- [Azure](#azure)
- [Provider-agnostic red flags](#provider-agnostic-red-flags)

---

## How replacement shows up

- **Text plan:** the resource is prefixed `-/+ resource "..." { ... }` with
  `# forces replacement` annotated on the offending attribute line. Grep for
  `forces replacement` and `must be replaced`.
- **JSON plan:** `.change.actions == ["delete","create"]` (or
  `["create","delete"]` if `create_before_destroy` is set). `.change.replace_paths`
  lists the attribute path(s) that forced it.
- **`prevent_destroy`:** if a resource with `lifecycle { prevent_destroy = true }`
  is slated to be destroyed or replaced, the apply **errors out** before doing
  anything. That's a misconfiguration finding, not just a risk — the change can't
  proceed as written.

---

## GCP

### Stateful — replace or destroy = data loss (usually Critical)
| Resource | Notes |
|----------|-------|
| `google_firestore_database` | Holds all documents. `delete_protection_state = "DELETE_PROTECTION_ENABLED"` blocks deletion (apply errors). `location_id`, `type`, `database_edition` force replacement. Recreating = total data loss; no undo. |
| `google_storage_bucket` | Holds objects. Destroy fails unless `force_destroy = true` (so `force_destroy=false` + a destroy = apply error; `force_destroy=true` + destroy = silent deletion of all objects — worse). `name`, `location` force replacement. |
| `google_bigquery_dataset` / `google_bigquery_table` | Hold data. `deletion_protection` (table) blocks destroy. Changing `location`, dataset `dataset_id`, or many schema ops force replacement. |
| `google_sql_database_instance` | `deletion_protection` defaults on. `region`, `database_version` (downgrades), `name` force replace = total DB loss. |
| `google_secret_manager_secret` | Replacing the secret destroys all versions. `secret_id`, `replication` force replacement. |
| `google_redis_instance`, `google_filestore_instance`, `google_compute_disk` | Stateful; region/zone/tier changes often force replace, losing data. |
| `google_pubsub_topic` | Replacing/destroying a topic drops in-flight + un-acked messages and orphans subscriptions. `name` forces replace. |

### Breaking production (downtime / connectivity / access)
| Resource | Why it bites |
|----------|--------------|
| `google_cloud_run_v2_service` / `_job` | New revision on most changes (usually safe, traffic shifts gradually). But replacement, a bad image, removed env/secret a container needs, or `min_instance_count → 0` causes errors or cold-start latency. Removing a mounted secret/env that the app reads = runtime crash, not a plan error. |
| `google_*_iam_member` / `_binding` | **Removing** a binding revokes access a live workload may depend on — e.g. dropping a worker SA's `storage.objectAdmin` causes 403s on upload at runtime, invisible in the plan. `_binding` is authoritative (replaces the whole policy); `_member` is additive. Confusing the two silently strips other members. |
| `google_compute_firewall`, `google_compute_subnetwork`, `google_network_security_*`, `google_network_services_gateway` | Egress/ingress and proxy rule changes can sever a running service's connectivity (e.g. an allowlist that no longer covers an endpoint the app calls). |
| `google_dns_record_set`, `google_dns_managed_zone` | TTL means changes/removals propagate slowly; a wrong/removed record breaks resolution for the TTL window. |
| `google_cloud_scheduler_job`, `google_cloud_tasks_queue` | Replacing/pausing stops triggers; queue replacement can drop queued tasks. |
| `google_project_service` (API enablement) | Disabling an API (a destroy here) can break every resource that depends on it. |

### Future problems
- `lifecycle_rule { action { type = "Delete" } condition { age = N } }` on a
  bucket quietly deletes objects older than N days — correct by design, but worth
  surfacing if data retention matters.
- Computed/interpolated `name`s (e.g. `${project}-${alias}-art`) risk colliding
  or exceeding length limits (GCS bucket ≤ 63 chars; many GCP names ≤ 63). A
  `precondition` guarding this is a sign the author already hit it.
- Resources created without `prevent_destroy` that would be catastrophic to lose.
- Per-revision churn: optional env/labels that get re-emitted differently each
  run, recreating a Cloud Run revision on every apply for no reason.

---

## AWS

### Stateful — replace/destroy = data loss
- `aws_db_instance`, `aws_rds_cluster` — `deletion_protection`,
  `skip_final_snapshot`. Changing `engine_version` (downgrade), `name`,
  `availability_zone` can force replace = DB loss.
- `aws_s3_bucket` — destroy fails unless `force_destroy=true`. `bucket` (name)
  forces replace. Replacing loses nothing if name unchanged, but a name change
  orphans/recreates.
- `aws_ebs_volume`, `aws_efs_file_system`, `aws_dynamodb_table`
  (`deletion_protection_enabled`), `aws_elasticache_cluster`,
  `aws_redshift_cluster` — stateful; size/AZ/engine changes often force replace.
- `aws_ecr_repository` — `force_delete` controls whether images block destroy.

### Breaking production
- `aws_instance` — `ami`, `instance_type` (some), `subnet_id`, user_data (with
  `user_data_replace_on_change`) force replacement = instance recreated.
- `aws_security_group` / `aws_security_group_rule` — removing rules cuts access;
  changing `name`/`vpc_id` forces replace and detaches.
- `aws_iam_role` / `aws_iam_policy` — removing/renaming revokes permissions a
  running workload needs.
- `aws_lb_target_group`, `aws_lb_listener`, `aws_route53_record` — replacement or
  removal interrupts traffic / resolution.
- `aws_lambda_function` — env var / layer / role removal breaks invocations at
  runtime, not at plan time.

---

## Azure

### Stateful — replace/destroy = data loss
- `azurerm_storage_account`, `azurerm_storage_container` — `name`, `location`,
  `account_tier` changes can force replace; holds blobs.
- `azurerm_mssql_database` / `_server`, `azurerm_postgresql_*`,
  `azurerm_cosmosdb_account` — stateful DBs; region/SKU/version changes force
  replace. Many support a delete-protection/`prevent_deletion_if_contains_data`.
- `azurerm_managed_disk`, `azurerm_key_vault` (and secrets within — soft-delete/
  purge protection matters).

### Breaking production
- `azurerm_linux_web_app` / `azurerm_app_service`, `azurerm_container_app` —
  app settings / connection string removal breaks the running app.
- `azurerm_network_security_group`, `azurerm_subnet`, `azurerm_firewall` —
  connectivity changes.
- `azurerm_role_assignment` — removal revokes access.
- `azurerm_dns_*` — TTL-delayed propagation.

---

## Provider-agnostic red flags

- **Any `["delete","create"]` on a resource whose name contains** `db`,
  `database`, `bucket`, `storage`, `state`, `vault`, `secret`, `volume`, `disk`,
  `table`, `topic`, `queue` — inspect closely for data loss.
- **`prevent_destroy` resource being destroyed/replaced** → apply will error;
  flag as a misconfiguration the author probably didn't intend.
- **A destroy with no corresponding create** of a stateful resource → data is
  simply going away. Confirm intent.
- **Module/count/for_each key changes** — changing a `for_each` key or `count`
  index can destroy-and-recreate resources that look unchanged, because their
  Terraform address moved. The plan shows it as destroy+create of "the same"
  resource. Easy to miss; very dangerous for stateful resources. A `moved {}`
  block is the safe alternative.
- **Provider version upgrades** in the same plan can change defaults and cause
  unexpected diffs — note when the provider/required_version changed alongside
  resource changes.
- **`sensitive` values shown as `(known after apply)`** can hide that a secret
  or credential is being rotated — a consumer that caches the old value breaks.
