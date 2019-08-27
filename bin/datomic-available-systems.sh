#!/usr/bin/env bash

echo "---- Here are all your Datomic systems in AWS:"

aws ec2 describe-instances --filters "Name=tag-key,Values=datomic:tx-group" "Name=instance-state-name,Values=running" --query 'Reservations[*].Instances[*].[Tags[?Key==`datomic:system`].Value]' --output text

echo "----"
