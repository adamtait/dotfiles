#!/usr/bin/env bash


echo -e "\n---- DNSCrypt blacklist generator"

# --- check usage + argument count
if [ $# == 0 ] || [ $# -gt 1 ]
then
    SCRIPT_NAME=$(basename "$0")
    echo ""
    echo "Use this script to generate a DNSCrypt compatible black list file."
    echo "Usage: $SCRIPT_NAME /PATH/TO/OUTPUT/FILE"
    echo "example: $SCRIPT_NAME \"~/.dnscrypt/domains.blacklist.txt\""
    echo ""
    exit 0
fi


echo "----- download.dnscrypt.info/blacklists/domains/mybase.txt"
curl -0L https://download.dnscrypt.info/blacklists/domains/mybase.txt > $1


echo "----- github.com/StevenBlack/fakenews-gambling-porn-social/hosts"
TMP_PATH=/tmp/domains.blacklist.StevenBlack.txt
curl -0L https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts > $TMP_PATH


echo "----- build domain black list at "
echo -e "\n\n\n\n\n" >> $1
sed \
    -e '/127\.0\.0\.1 localhost/,/End of custom host records\./d' \
    -e "s/0.0.0.0 //g" \
    $TMP_PATH >> $1
rm -f $TMP_PATH
