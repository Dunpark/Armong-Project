#!/usr/bin/env python
# coding: utf-8

# In[42]:


import requests

def get_reviews(appid, params={'json':1}):
        url = 'https://store.steampowered.com/appreviews/'
        response = requests.get(url=url+appid, params=params, headers={'User-Agent': 'Mozilla/5.0'})
        return response.json()
    
def get_n_reviews(appid, n):
    reviews = []
    cursor = '*'
    params = {
            'json' : 1,
            'filter' : 'all',
            'language' : 'english',
            'day_range' : 9223372036854775807,
            'review_type' : 'all',
            'purchase_type' : 'all'
            }

    while n > 0:
        params['cursor'] = cursor.encode()
        params['num_per_page'] = 100
        n -= 100

        response = get_reviews(str(appid), params)
        cursor = response['cursor']
        reviews += response['reviews']

        if len(response['reviews']) < 100: break

    return reviews


# In[43]:


app_id = 1562700
n_reviews = 1000  # Set the desired number of reviews

reviews = get_n_reviews(app_id, n_reviews)

# Now 'reviews' contains the list of reviews for the specified app_id
print(f"Number of reviews retrieved: {len(reviews)}")


# In[44]:


# You can iterate through the reviews and print or process them as needed
all_reviews = []

for review in reviews:
    review_id = review['recommendationid']
    review_content = review['review']

    # Append the review information to the list
    all_reviews.append({
        'Review ID': review_id,
        'Review Content': review_content
    })
# Now 'all_reviews' contains the review information in a list of dictionaries
all_reviews  


# In[51]:


all_reviews_text = [review['Review Content'] for review in all_reviews]

# Now 'all_reviews_text' contains only the review text content
all_reviews_text #Some text are in other languages but not that many *limitations


# In[52]:


len(all_reviews_text)


# In[58]:


# Generate a timestamp for a unique file name
file_path = f'sanabi_pos_text.txt'
        
# Open the file in write mode
with open(file_path, 'w', encoding='utf-8') as file:
    # Write each element of the list to a new line in the file
    for item in all_reviews_text:
        file.write("%s\n" % item)

# Confirm that the file has been saved
print(f'All reviews have been saved to: {file_path}')


# In[ ]:




