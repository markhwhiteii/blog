# prep -------------------------------------------------------------------------
import pandas as pd
from sentence_transformers import SentenceTransformer

dat = pd.read_csv('ratings_coded.csv')
text = dat['text']

# get embeddings ---------------------------------------------------------------
model = SentenceTransformer('answerdotai/ModernBERT-base')
embeddings = model.encode(text)

dat_embeddings = pd.DataFrame(embeddings).add_prefix('embed_')
dat_out = pd.concat([dat, dat_embeddings], axis=1)

dat_out.to_csv('ratings_embedded.csv', index=False)
