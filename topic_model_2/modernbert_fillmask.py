# prep -------------------------------------------------------------------------
from transformers import pipeline
import pandas as pd

dat = pd.read_csv('ratings_coded.csv')
dat = dat[dat['visual_style'] != 9]
dat['masked_class'] = ''

# fill mask --------------------------------------------------------------------
pipe = pipeline('fill-mask', model='answerdotai/ModernBERT-base')

for i in dat.index:
  review = dat['text'][i].lower()
  
  prompt = f'''this is a movie review: {review}
  question: did the review discuss the visual style of the film, yes or no?
  answer:[MASK]'''
  
  out = pipe(prompt)
  
  tmp = out[0]['token_str'].strip().lower()
  
  if tmp not in ['yes', 'no']:
      tmp = out[1]['token_str'].strip().lower()
  
  dat.loc[i, 'masked_class'] = tmp

dat.to_csv('ratings_masked.csv')
