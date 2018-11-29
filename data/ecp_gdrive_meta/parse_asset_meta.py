from __future__ import print_function
import json
import re
import csv
import datetime
from pprint import pprint

with open("gdrive.json") as f:
    ecp = json.load(f)

csvf =  open("file_counts.csv", "w")
csvw = csv.writer(csvf, delimiter=",")
csvw.writerow([
    "province", "constituency_code", "n_pdfs", "n_pdfs_before_june_25",
    "n_compressed_files", "n_compressed_files_before_june_25",  "n_other_files",
    "n_other_files_before_june_25", "folder_name"
])



# Create constituency level data
# Sindh has reliable file names in
# 1YeksTmpompRjBrlr2vcNb2IMJ-ZvAb9W (PS)
# 1v7Iec5cyYuPSMeUhr1kVZw5Bc8KggfR7 (NA)

# Punjab also looks reliable

# Balochistan fine except for NA-264 Quetta-I and NA-266 Quetta-III

out_list = []
for province, province_dat in ecp.iteritems():
    print(province)
    print(len(province_dat["items"]))

    # Find constituency_folders:
    constituency_ids = {
        item["id"]: {
            "constituency_code": re.findall(r"[NP][APBKS]\-\s*[0-9]+", item["name"])[0],
            "name": item["name"],
            "pdfs": 0,
            "pdfs_before_june_25": 0,
            "compressed_files": 0,
            "compressed_files_before_june_25": 0,
            "other_files": 0,
            "other_files_before_june_25": 0,
        }
        for item in province_dat["items"]
        if item["mimeType"] == "application/vnd.google-apps.folder" and
        re.search(r"[NP][APBKS]\-\s*[0-9]+", item["name"])
    }

    for item in province_dat["items"]:
        if item["parents"][0] in constituency_ids:
            if item["mimeType"] == "application/pdf":
                key = "pdfs"
            elif item["mimeType"] in ["application/java-archive", "application/zip"]:
                key = "compressed_files"
            else:
                key = "other_files"
                
            constituency_ids[item["parents"][0]][key] += 1
            if datetime.datetime.strptime(item["modifiedTime"][0:18], "%Y-%m-%dT%H:%M:%S") < datetime.datetime(2018, 6, 26, 0, 0, 0):
                constituency_ids[item["parents"][0]][key + "_before_june_25"] += 1

    for key, dat in constituency_ids.iteritems():
        csvw.writerow([
            province, dat["constituency_code"], dat["pdfs"], dat["pdfs_before_june_25"],
            dat["compressed_files"], dat["compressed_files_before_june_25"],
            dat["other_files"], dat["other_files_before_june_25"], dat["name"]
        ])
 #    for constituency, folder_id in constituency_ids.iteritems():
 #       for item in 

csvf.close()
