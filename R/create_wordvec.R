library(wordVectors)



#prep_word2vec(origin="full_lsa_text.txt", destination = "test_data4.txt", lowercase=TRUE, bundle_ngrams = 2)
t1 <- train_word2vec("full_lsa_text.txt", output_file = "wordvec_ndcs_new.bin", vectors=300, threads=4,
                     window = 12, classes = 0, cbow = 0, min_count = 15, iter = 90, force=TRUE, negative_samples = 15)