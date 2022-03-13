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
    return df


def delete_question_rows(df):
    df2 = df[df['correct_answer'].isna()]
    return df2


def create_columns_word_number_word_rt(df):
    df2 = df.drop('phrase', axis=1).join(df['phrase'].str.split(' ', expand=True).stack().reset_index(level=1,
                                                                                                      drop=True).rename(
        'word'))
    df2 = df2.reset_index()
    df2['word_number'] = 0
    initial_row_setting = df2['setting'].iloc[0]
    count = 1
    for i, row in df2.iterrows():
        if row['setting'] != initial_row_setting:
            initial_row_setting = row['setting']
            count = 1
        df2.at[i, 'word_number'] = count
        count += 1

    df2 = pd.concat([df['phrase'], df2], axis=1)

    df3 = df.drop('times', axis=1).join(df['times'].str.split('|', expand=True).stack().reset_index(level=1,
                                                                                                    drop=True).rename(
        'rt'))
    df3 = df3['rt']
    df3 = df3.reset_index()

    return pd.concat([df2, df3], axis=1)


def create_region(df):
    df['region'] = "-"
    for i, row in df.iterrows():
        if row['setting'] == 'focused':
            if row['word_number'] == 1:
                df.at[i, 'region'] = 'focus_particle'
            elif row['word_number'] in [2, 3]:
                df.at[i, 'region'] = 'quantifier_f'
            elif row['word_number'] in [4, 5]:
                df.at[i, 'region'] = 'ntw_1'
            elif row['word_number'] in [6, 7]:
                df.at[i, 'region'] = 'ntw_2'
        elif row['setting'] == 'scalar':
            if row['word_number'] in [1, 2]:
                df.at[i, 'region'] = 'quantifier_s'
            elif row['word_number'] in [3, 4]:
                df.at[i, 'region'] = 'ntw_1'
            elif row['word_number'] in [5, 6]:
                df.at[i, 'region'] = 'ntw_2'
        elif row['setting'] == 'cancelation':
            if row['word_number'] in [1, 2]:
                df.at[i, 'region'] = 'in_fact'
            elif row['word_number'] in [3, 4]:
                df.at[i, 'region'] = 'they_all'
            elif row['word_number'] == 5:
                df.at[i, 'region'] = 'did'
            elif row['word_number'] == 6:
                df.at[i, 'region'] = 'word6'
        elif row['setting'] == 'complement':
            if row['word_number'] in [1, 2]:
                df.at[i, 'region'] = 'anaphor'

            if row['itemID'] in [2, 10, 21, 23, 24]:
                if row['word_number'] in [3, 4, 5]:
                    df.at[i, 'region'] = 'predicate'
                elif row['word_number'] == 6:
                    df.at[i, 'region'] = 'clause_boundary'
                elif row['word_number'] in [7, 8]:
                    df.at[i, 'region'] = 'ntw_1'

            elif row['itemID'] == 4:
                if row['word_number'] in [3, 4, 5, 6]:
                    df.at[i, 'region'] = 'predicate'
                elif row['word_number'] == 7:
                    df.at[i, 'region'] = 'clause_boundary'
                elif row['word_number'] in [8, 9]:
                    df.at[i, 'region'] = 'ntw_1'

            elif row['itemID'] == 8:
                if row['word_number'] in [3, 4, 5, 6, 7, 8, 9, 10]:
                    df.at[i, 'region'] = 'predicate'
                elif row['word_number'] == 11:
                    df.at[i, 'region'] = 'clause_boundary'
                elif row['word_number'] in [12, 13]:
                    df.at[i, 'region'] = 'ntw_1'

            elif row['itemID'] == 20:
                if row['word_number'] in [3, 4, 5, 6, 7]:
                    df.at[i, 'region'] = 'predicate'
                elif row['word_number'] == 8:
                    df.at[i, 'region'] = 'clause_boundary'
                elif row['word_number'] in [9, 10]:
                    df.at[i, 'region'] = 'ntw_1'

            else:
                if row['word_number'] in [3, 4]:
                    df.at[i, 'region'] = 'predicate'
                elif row['word_number'] == 5:
                    df.at[i, 'region'] = 'clause_boundary'
                elif row['word_number'] in [6, 7]:
                    df.at[i, 'region'] = 'ntw_1'

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

    """Delete question rows"""
    df = delete_question_rows(df)

    """Create columns [word_number, word, rt]"""
    df = create_columns_word_number_word_rt(df)

    """Create columns [region_number]"""
    df = create_region(df)

    """Only keep necessary columns"""
    df_processed = df[['setting', 'submission_id', 'word_number', 'word', 'rt', 'phrase', 'region',
                       'type', 'phrase_length']]

    print(df_processed)

    """Save the processed csv file"""
    df_processed.to_csv(save_path)


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
