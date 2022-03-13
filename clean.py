import pandas as pd


def read_csv(path):
    df = pd.read_csv(path)
    # print(df)
    # print(df.columns)
    # print(df.submission_id.unique())
    return df


def get_averaged_accuracy(df):
    # This function should be used in the raw csv because "response" column is removed in the processed one.
    response_count = 0
    correct_count = 0
    for index, row in df.iterrows():
        if isinstance(row['correct_answer'], str):
            response_count += 1
            if row['correct_answer'] == row['response']:
                correct_count += 1
    accuracy = correct_count / response_count
    print("The averaged accuracy is: ", accuracy)


def remove_filler(df):
    df = df[~df.itemID.str.contains('f')]
    df = df.astype({"itemID": int})
    return df


def get_items(df):
    items = df.itemID.unique().tolist()
    items.sort()
    return items

def save_csv(save_path):
    df.to_csv(save_path)


def filter_individual_accuracy(df):
    submissions = df.submission_id.unique()
    items_count = len(get_items(df))
    correct_count = 0
    submissions_new = []
    for s in submissions:
        for index, row in df.iterrows():
            if isinstance(row['submission_id'], int) and row['submission_id'] == s:
                if row['correct_answer'] == row['response']:
                    correct_count += 1
        accuracy = correct_count / items_count
        print("The local accuracy for participant", s, "is: ", accuracy)
        if accuracy >= 0.8:
            submissions_new.append(s)
        correct_count = 0
    df = df[df['submission_id'].isin(submissions_new)]
    print(df)
    return df


if __name__ == "__main__":
    csv_path = 'https://magpie-demo.herokuapp.com/experiments/301/retrieve'
    save_path = 'data_processed.csv'

    """Read the raw CSV"""
    df = read_csv(csv_path)

    """Get the average accuracy for all the participants"""
    get_averaged_accuracy(df)

    """Get individual accuracy, only keep those bigger than 80%"""
    df = filter_individual_accuracy(df)

    """Remove all the filler sentences"""
    df = remove_filler(df)

    """Get all the items"""
    items = get_items(df)
    print("All the items are: ", items)

    """Save the processed csv file"""
    save_csv(save_path)






    """
    # Extreme individual RTs (greater than 1,500 ms or less than 100 ms) were trimmed (0.3% of all data).
    # """
    # for index, row in df.iterrows():
    #     if isinstance(row['times'], str):
    #         times = row['times'].split('|')
    #         for t in times:
    #             if int(t) < 100 or int(t) > 1500:
    #                 print("Wrong!!!!")
    #                 print(row)
    #                 print('=== === ===')
    #
    # """
    # Get all the phrases to have a look for grouping/splitting RTs
    # """
    # df_read_phrases = df[['phrase', 'setting', 'times']]
    # print(df_read_phrases.setting.unique())
    # print("____________________________")
