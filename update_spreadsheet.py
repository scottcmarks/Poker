from __future__ import print_function
from googleapiclient.discovery import build
from httplib2 import Http
from oauth2client import file, client, tools
from oauth2client.file import Storage
import csv

# If modifying these scopes, delete the file token.json.
SCOPES = 'https://www.googleapis.com/auth/spreadsheets'
CREDENTIALS_FILE = 'token.json'
CLIENT_SECRET_FILE = 'client_secret.json'
API = 'sheets'
API_VERSION = 'v4'

# The ID and range of a sample spreadsheet.
POKER_SPREADSHEET_ID = '1ZFNBorOUulqXVu_UspAHK1gm4436tf8XszWw9CKSAiI'
POKER_RANGE_NAME = 'Data Entry!Data'
POKER_DATA_FILE = 'Data Entry.csv'
SHOW_DATA_FILE_DATA = False # True
READ_OLD_DATA_AND_COMPARE = False # True
CLEAR_OLD_DATA =  True
SHOW_DATA_CLEARING_RESULTS = False # True
SHOW_WRITING_RESULTS = False # True

def batch_update_values(self, spreadsheet_id, range_name,
                        value_input_option, values):
    service = self.service
    data = [
        {
            'range': range_name,
            'values': values
        },
    ]
    body = {
        'valueInputOption': value_input_option,
        'data': data
    }
    result = service.spreadsheets().values().batchUpdate(spreadsheetId=spreadsheet_id, body=body).execute()
    print('{0} cells updated.'.format(result.get('updatedCells')))
    return result
    
def main():
    """Shows basic usage of the Sheets API.
    Prints values from a sample spreadsheet.
    """
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    store = Storage(CREDENTIALS_FILE)
    creds = store.get()
    if not creds or creds.invalid:
        flow = client.flow_from_clientsecrets(CLIENT_SECRET_FILE, SCOPES)
        creds = tools.run_flow(flow, store)
        print( f'Storing credentials to {CREDENTIALS_FILE}' )
    service = build(API, API_VERSION, http=creds.authorize(Http()))

    # Call the Sheets API
    sheet = service.spreadsheets()

    if READ_OLD_DATA_AND_COMPARE:
        print( f'Reading values currently in spreadsheetId={POKER_SPREADSHEET_ID}')
        result = sheet.values().get(spreadsheetId=POKER_SPREADSHEET_ID,range=POKER_RANGE_NAME).execute()
        print( f'result={result}' )
        data_from_spreadsheet = result.get('values', [])
        if not data_from_spreadsheet:
            print(f'No current data found.')
        print('')

    if CLEAR_OLD_DATA:
        result = sheet.values().clear(spreadsheetId=POKER_SPREADSHEET_ID,range=POKER_RANGE_NAME).execute()
        if SHOW_DATA_CLEARING_RESULTS:
            print( f'Cleared values currently in spreadsheetId={result.get("spreadsheetId","???")} range={result.get("clearedRange","???")}')
            print('')

    if READ_OLD_DATA_AND_COMPARE:
        print( f'Reading values in spreadsheetId={POKER_SPREADSHEET_ID} after clearing')
        result = sheet.values().get(spreadsheetId=POKER_SPREADSHEET_ID,range=POKER_RANGE_NAME).execute()
        print( f'result={result}' )
        print('')

    with open(POKER_DATA_FILE) as csv_file:
        csv_reader = csv.reader(csv_file, delimiter=',')
        data_from_csv_file = [row for row in csv_reader]

    if SHOW_DATA_FILE_DATA:
        print( f'Rows read from {POKER_DATA_FILE}:' )
        for row in data_from_csv_file:
            print( f'{row[0]} {row[1]} {row[2]}' )

    body = {
        'values': data_from_csv_file
    }
    
    # print('')
    
    # print(f'Writing to spreadsheetId={POKER_SPREADSHEET_ID}')
    # print(f'body={body}')
    result = sheet.values().update(
        spreadsheetId=POKER_SPREADSHEET_ID,
        range=POKER_RANGE_NAME,
        valueInputOption='USER_ENTERED',
        body=body).execute()

    if SHOW_WRITING_RESULTS:
        print( f'Updated values in spreadsheetId={result.get("spreadsheetId","???")} range={result.get("updatedRange","???")}')
        print( f'Updated {result.get("updatedRows")} rows X {result.get("updateColumns")} columns = {result.get("updatedCells")} cells.')
        print( f'Result from writing to spreadsheetId={POKER_SPREADSHEET_ID}')
        print('')

    if READ_OLD_DATA_AND_COMPARE:
        print( f'Reading updated values in spreadsheetId={POKER_SPREADSHEET_ID}')
        result = sheet.values().get(spreadsheetId=POKER_SPREADSHEET_ID,range=POKER_RANGE_NAME).execute()
        print( f'result={result}' )
        data_from_spreadsheet = result.get('values', [])

        if not data_from_spreadsheet:
            print(f'No get data found.')
        else:
            for row in data_from_spreadsheet:
                # Print columns A-C, which correspond to indices 0-2 .
                print(f'{row[0]} {row[1]} {row[2]}')


            print('')

            if (data_from_spreadsheet == data_from_csv_file):
                print( f'Data updated and verified.')
            else:
                print (f'Error updating data')
                print( f'data_from_csv_file={data_from_csv_file}' )
                print( f'data_from_spreadsheet={data_from_spreadsheet}' )
                print( f'data_from_csv_file == data_from_spreadsheet is {data_from_csv_file == data_from_spreadsheet}' )
        print('')

if __name__ == '__main__':
    main()
