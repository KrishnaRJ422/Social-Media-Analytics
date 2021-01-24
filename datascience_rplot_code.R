datascience<- search_tweets("#datascience   -filter:quote -filter:retweets -filter:replies",n=5000,lang='en',include_rts = FALSE)

datasciencetxt<-datascience$text

#remove urls in text field
datascienceurl <-rm_twitter_url(datasciencetxt)

#remove spl chars
datasciencespl <- gsub("[^A-Za-z0-9]", " ",datascienceurl)

datascience_corpus<-datasciencespl %>% 
  VectorSource() %>%
  Corpus()
datascience_corpus[[3]]$content
#converting to lowercase
datascience_corpus_lwr<- tm_map(datascience_corpus,tolower)
datascience_corpus_lwr[[3]]$content
#removing stop words in english
datascience_corpus_stpwrd<- tm_map(datascience_corpus_lwr,removeWords,stopwords("english"))
datascience_corpus_stpwrd[[3]]$content
#removing white spaces
datascience_corpus_final<-tm_map(datascience_corpus_stpwrd,stripWhitespace)
datascience_corpus_final[[3]]$content
#term frequency
term_count<- freq_terms(datascience_corpus_final,20)
custom_words<- c("can","one","an","a","u","f","m","th","k","e","amp","the","s","and","may","will","m","us","like","shall","t","just")
term_count_clean<- tm_map(datascience_corpus_final,removeWords,custom_words)
term_count_clean_new<-freq_terms(term_count_clean,50)
#plot top popular terms
term_10<- data.frame(subset(term_count_clean_new,FREQ>10))
ggplot(term_10,aes(x=reorder(WORD,-FREQ),y=FREQ))+geom_bar(stat="identity",fill="blue")+theme(axis.text.x=element_text(angle=45,hjust=1))

#ggplot(datascience_corpus_stpwrd,aes(x=reorder(WORD,-FREQ),y=FREQ))+geom_bar(stat="identity",fill="blue")+theme(axis.text.x=element_text(angle=45,hjust=1))
#word cloud
#wordcloud(words=term_count_clean_new$WORD,freq=term_count_clean_new$FREQ,min.freq=3,max.words=Inf,colors=brewer.pal(6,"Accent"),scale=c(2,0.5),random.order=FALSE,colorblindFriendly=TRUE)
#figPath = system.file("C:\Users\krish\OneDrive\Desktop\twitter_logo.png",package = "wordcloud2")
wordcloud2(term_count_clean_new, figPath = "C:/Users/krish/OneDrive/Desktop/twitter_logo.png" )
wordcloud2(term_count_clean_new,size=1,minSize=0.5,rotateRatio=0.6,fontWeight = 'normal',fontFamily='Arial', color='random-dark',shuffle=TRUE,minRotation = -pi/4,backgroundColor = 'white')
letterCloud(term_count_clean_new,word='R')
