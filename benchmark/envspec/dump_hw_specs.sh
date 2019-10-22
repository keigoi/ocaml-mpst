# ./dump_hw_specs.sh >hwspecs.txt 2>&1
set -v
#####

cat /etc/lsb-release
#####

cat /etc/issue.net
#####

cat /etc/debian_version
#####

uname -a
#####

lsb_release -a
#####

lscpu
#####

cat /proc/cpuinfo
#####

sudo lshw -class processor
#####

sudo dmidecode
