# TalkingData
Final presentation for Applied Data Mining

From Kaggle Competition:

https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection

"Fraud risk is everywhere, but for companies that advertise online, click fraud can happen at an overwhelming volume, resulting in misleading click data and wasted money. Ad channels can drive up costs by simply clicking on the ad at a large scale. With over 1 billion smart mobile devices in active use every month, China is the largest mobile market in the world and therefore suffers from huge volumes of fraudulent traffic.
TalkingData, China’s largest independent big data service platform, covers over 70% of active mobile devices nationwide. They handle 3 billion clicks per day, of which 90% are potentially fraudulent. Their current approach to prevent click fraud for app developers is to measure the journey of a user’s click across their portfolio, and flag IP addresses who produce lots of clicks, but never end up installing apps. With this information, they've built an IP blacklist and device blacklist.
While successful, they want to always be one step ahead of fraudsters and have turned to the Kaggle community for help in further developing their solution. In their 2nd competition with Kaggle, you’re challenged to build an algorithm that predicts whether a user will download an app after clicking a mobile app ad. To support your modeling, they have provided a generous dataset covering approximately 200 million clicks over 4 days!"

Researched from forums and other discussions on Kaggle perform analysis and to create this presentation

Click farms may also be used by app developers to inflate their user metrics to fool investors and shareholders. 

https://kotaku.com/inside-chinese-click-farms-1795287821

EDA using PCA, kproto clusters from clustMixType, apriori association rules.

Binomial Classification using Naive Bayes and Decision Tree. Experiment with Cross validation and use different sampling techniques offered by caret package - over, down, smote with
decision tree and naive bayes to train the models.

Easy to understand visuals using ggplot and related libraries for EDA and classification models 
