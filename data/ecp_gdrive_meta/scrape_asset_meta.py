from __future__ import print_function

import json

from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools

SCOPES = 'https://www.googleapis.com/auth/drive.metadata.readonly'

def find_items(parent_ids, storage_dict, service):

    query_per_id = ["'" + parent_id + "' in parents" for parent_id in parent_ids]
    query = " or ".join(query_per_id)
    print(query)
        
    results = service.files().list(
        q=query,
        pageSize=1000,
        fields="nextPageToken, files(id, name, mimeType, description, parents, createdTime, modifiedTime, owners, size, fileExtension)"
    ).execute()

    
    items = results.get('files', [])
#    print("Number of files found:")
#    print(len(items))
#    print("Page token:")
#    print(results.keys())
    
    folders_found = []
    if items:
        for item in items:
            storage_dict["items"].append(item)
            if item["mimeType"] == "application/vnd.google-apps.folder":
                find_items([item['id']], storage_dict, service)
#            folders_found.append(item['id'])
#            print(u'{0} ({1})'.format(item['name'], item['id']))
#        if len(folders_found):
#            find_items(folders_found, storage_dict, service)


def main():
    """Shows basic usage of the Drive v3 API.
    Prints the names and ids of the first 10 files the user has access to.
    """
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    store = file.Storage('token.json')
    creds = store.get()
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets('credentials.json', SCOPES)
        creds = tools.run_flow(flow, store)
    service = build('drive', 'v3', http=creds.authorize(Http()))

    province_dict = {
        "Khyber Pakhtunkhwa": {"id": "1-9EyLMWVoy6aibLlvtAZQrXxp_dHm5RC", "items": []},
        "Balochistan": {"id": "1wbF4TEMHL8s1rtL84R1P-Fr89RgtYdp1", "items": []},
        "Sindh": {"id": "1uz8j4fCP83iV98QGqZkPNAAbhQeE57-q", "items": []},
        "Punjab": {"id": "15nGH_z8H25fOh9h9df-3bqt5-NelC31v", "items": []}
    }                

    for province, province_dat in province_dict.iteritems():
        print("NOW ON PROVINCE: %s" % province)
        # Call the Drive v3 API
        parent_ids = [province_dat["id"]]
        find_items(parent_ids, province_dat, service)
        print("PROVINCE: %s" % province)
        print("FOUND ITEMS: %s" % str(len(province_dat["items"])))

    with open("gdrive.json", "w") as f:
        json.dump(province_dict, f, indent=2)

if __name__ == '__main__':
    main()

