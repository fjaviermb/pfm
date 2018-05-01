
import getopt,sys, os, re, json
from multiprocessing import Pool, cpu_count
from datetime import datetime
import socket
import struct

# This script parses file contining attacks and generate an output as a CSV

# num seconds to convert from EST to PST
ADJUSTMENT_TIME = 3*3600
# Changed to convert to GMT
ADJUSTMENT_TIME = 3*3600 + (-5 * 3600)


def datetime_to_tstamp(date, time, adjust=True):
    """Convert date and time strings into a timestamp matching the pcap files
    Inputs:
      - date string with the format: MM/DD/YYYY
      - time string with the format: HH:MM:SS

    Returns:
      - float timestamp
    """
    date_fmat = r'(?P<month>.{2})/(?P<day>.{2})/(?P<year>.{4})'
    time_fmat = r'(?P<hour>.{2}):(?P<min>.{2}):(?P<sec>.{2})'
    d = re.search(date_fmat, date)
    if (d is None):
        print >> sys.stderr, "ERROR: datetime_to_tstamp: invalid date string: {}".format(date)
        return
    t = re.search(time_fmat, time)
    if (t is None):
        print >> sys.stderr, "ERROR: datetime_to_tstamp: invalid time string: {}".format(time)
        return
    dt = datetime(int(d.group('year')), int(d.group('month')), int(d.group('day')), int(t.group('hour')), int(t.group('min')), int(t.group('sec')))
    if adjust:
        result = float(dt.strftime("%s")) - ADJUSTMENT_TIME
    else:
        result = float(dt.strftime("%s"))
    return result


def tstamp_to_datetime(tstamp):
    """Converts a pcap timestamp to date and time
    Input:
      - float timestamp

    Returns:
      - list where first element is date string: MM/DD/YYYY
      - second element is time string: HH:MM:SS
    """
    result = datetime.fromtimestamp(tstamp + ADJUSTMENT_TIME).strftime('%m/%d/%Y %H:%M:%S')
    return result.split()

def dur_to_sec(dur):
    """Convert duration string with format HH:MM:SS to float
    """
    dur_fmat = r'(?P<hour>.{2}):(?P<min>.{2}):(?P<sec>.{2})'
    d = re.search(dur_fmat, dur)
    if (d is None):
        print >> sys.stderr, "ERROR: dur_to_sec: invalid duration string"
        return
    return float(d.group('hour'))*3600 + float(d.group('min'))*60 + float(d.group('sec'))

"""
First packet times for each day of testing:

start_time_map = {
    '03/29/1999':('08:00:02', 9.22712402005345940589904785156250e+08),
    '03/31/1999':('08:00:09', 9.22885209075160026550292968750000e+08),
    '04/01/1999':('08:00:01', 9.22971601356755018234252929687500e+08),
    '04/02/1999':('08:00:00', 9.23058000588664054870605468750000e+08),
    '04/05/1999':('08:00:02', 9.23313602809497952461242675781250e+08),
    '04/06/1999':('08:00:00', 9.23400000174049019813537597656250e+08),
    '04/07/1999':('08:00:00', 9.23486400744150996208190917968750e+08]),
    '04/08/1999':('08:00:00', 9.23572800784062981605529785156250e+08]),
    '04/09/1999':('08:00:04', 9.23659204991575956344604492187500e+08])
}
"""

def read_attack_file(filename):
    #    label_fmat = r'(?P<ID>[\d]+\.\d{6})(?P<date>\d{2}/\d{2}/\d{4}) (?P<time>\d{2}:\d{2}:\d{2})  (?P<duration>\d{2}:\d{2}:\d{2}) (?P<dstIP>\d{3}\.\d{3}\.\d{3}\.\d{3})(?P<name>.{10}) (?P<insider>.{8}) (?P<manual>.{7}) (?P<console>.{7}) (?P<success>.{8}) (?P<aDump>.{6}) (?P<oDump>.{5}) (?P<iDumpBSM>.{9}) (?P<SysLogs>.{7}) (?P<FSListing>.{9}) (?P<StealthyNew>.{12}) (?P<Category>.*$)'
    label_fmat = r'(?P<ID>[\d]+\.\d{6})(?P<date>\d{2}/\d{2}/\d{4}) (?P<time>\d{2}:\d{2}:\d{2})  (?P<duration>\d{2}:\d{2}:\d{2}) (?P<dstIP>\d*\.\d*\.\d*\.[\d\*]*)(?P<name>[^ ]*) .*'
    with open(filename) as f:
        contents = f.read()
    matches = re.finditer(label_fmat, contents, re.M)
    attack_list, num_unique_attacks = make_attack_list(matches)
    return attack_list, num_unique_attacks


def make_attack_list(matches):
    result = []
    attackIDs = []
    attackNames = []
    for m in matches:
        attack = {}
        startTime = datetime_to_tstamp(m.group('date'), m.group('time'))
        endTime = startTime + dur_to_sec(m.group('duration'))
        attack['range'] = (startTime, endTime)
        attack['dstIP'] = m.group('dstIP')
        attack['name'] = m.group('name')
        attack['ID'] = m.group('ID')
        result.append(attack)
        if m.group('ID') not in attackIDs:
            attackIDs.append(m.group('ID'))
        if m.group('name') not in attackNames:
            attackNames.append(m.group('name'))

    return result, len(attackIDs)


# Name of the file containing which captures are attacks
MASTER_ATTACK_FILE = 'master-listfile-condensed.txt'

# Some IPs on attack file uses wildcards instead a particular IP
# This functions return if the current ip it is not or i a wildcard case. At the moment wilcard cases are not taken into an account. THis needs to be update.
def isIPWithuotWildcards(ip):
    return str(ip).count("*") == 0

# Converts IP string to number format
def ip2long(ip):
    """
    Convert an IP string to long
    """
    n1,n2,n3,n4=ip.split(".")
    n1 = str(int(n1))
    n2 = str(int(n2))
    n3 = str(int(n3))
    n4 = str(int(n4))

    packedIP = socket.inet_aton(n1+"."+n2+"."+n3+"."+n4)
    return struct.unpack("!L", packedIP)[0]

def main(argv):

    raw_dir = ""
    dataset_dir = ""
    c="|"

    try:
        opts, args = getopt.getopt(argv,'s:r:a:')
    except getopt.GetoptError:
        print("error")
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-s", "--separator"):
            separator=arg
        elif opt in ("-r","--rawdir"):
            raw_dir=arg
        elif opt in ("-a","--datasetdir"):
            dataset_dir=arg

    filename = MASTER_ATTACK_FILE
    filedir=dataset_dir+"/"
    input_file=filedir+"/"+filename

    attack_list, num_unique_attacks = read_attack_file(input_file)

    # Print output as a CSV. Fields:
    # - ID
    # - destination IP as number
    # - Range Start: start attack time
    # - Range End: end attack time
    # - destination IP as string
    # - name of the he attack
    numIssues = 0
    for line in attack_list:
        # {'dstIP': '172.016.112.050', 'name': 'ps', 'range': (922677515.0, 922677762.0), 'ID': '41.084031'}
        if isIPWithuotWildcards(line['dstIP']):
            lineStr = line['ID'] + separator + str(ip2long(line['dstIP'])) + separator + str(line['range'][0]) + separator + str(
                line['range'][1]) + separator + line['dstIP'] + separator + line['name']
            print(lineStr)
        else:
            numIssues = numIssues + 1
            sys.stderr.write("TODO: found IP with wildcard: " + line['dstIP'] + "\n")
    if numIssues > 0:
        sys.stderr.write("Num issues/problems founds: " + str(numIssues))



if __name__ == '__main__':
    main(sys.argv[1:])
