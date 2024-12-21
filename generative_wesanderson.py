# prep -------------------------------------------------------------------------
import re
import pandas as pd
from openai import OpenAI

API_KEY=''
model='gpt-4o-mini'
client = OpenAI(api_key=API_KEY)

# get description --------------------------------------------------------------
description = client.chat.completions.create(
  model=model, 
  messages=[
    {
      'role': 'user', 
      'content': 'Provide a brief description of director Wes Anderson\'s visual style.'
    }
  ],
  temperature=0
)

description = description.choices[0].message.content

with open('description.txt', 'w') as file:
  file.write(description)

with open('description.txt', 'r') as file:
  description = file.read()

# define coding fn -------------------------------------------------------------
job = '''You are a helpful research assistant who is classifying movie reviews.
Your job is to determine if a movie review discusses Wes Anderson's unique visual style.
'''

directions = '''
The user will provide a review, and you are to respond with one number only.
If the review discusses Wes Anderson's visual style at all, reply 1. If it does not, reply 0.
Do not give any commentary.'''

def get_code(review):
  messages = [
      {
          'role': 'system',
          'content': job + description + directions
      },
      {   
          'role': 'user',
          'content': review
      },
  ]
  
  response = client.chat.completions.create(
    model=model, 
    messages=messages,
    temperature=0
  )
    
  return(response.choices[0].message.content)

# code text --------------------------------------------------------------------
dat = pd.read_csv('ratings_coded.csv')
dat['gen_class'] = ''

for i in dat.index:
  dat.loc[i, 'gen_class'] = get_code(dat['text'][i])

dat.to_csv('ratings_generative.csv')
