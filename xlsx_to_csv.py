import glob
excel_files = glob.glob('~/Volumes/cchpc/raw2/e350/*.xlsx') # assume the path
for excel in excel_files:
    out = excel.split('.')[0]+'.csv'
    df = pd.read_excel(excel) # if only the first sheet is needed.
    df.to_csv(out) 