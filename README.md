# beerwinners Team Project: Text Mining with Recipes

1. Load recipe data with r script "1.Recipe Scraping.R"
Note: In order to replicate our results and test our clusters, making iterative changes to our code for optimization purposes, we have decided to load the data set "RecipeData.RDA".  This can be used by anyone who wishes to duplicate our results exactly.
2. Use tm R package to create and clean corpus with "2.Recipe Corpus Clean.R"
Note: You can see more exploratory code with ingredients clean up in the r script "Recipe Ingredients Exploratory.R"
3. Next, we want to create clusters using the k-means method.  Run the next r script to complete this step, "3.K-means Recipe Clustering.R"
4. We run Naive Bayes on our clusters to see if the classification of our "flavor profiles" works well, run the next step "4.NB_kmeans.R"

#Where we started

With five group members we came up with five independent ideas and vetted them separately.  

We liked the idea of using recipe data for a text mining project at first because we thought it would be useful and fun to come up with a way to find ingredients for a grocery list using an application that would give us the minimum distance to find the items in the grocery store.  This idea incorporated bodies of text (grocery lists) and distance (the euclidean distance we were striving to utilize in our algorithm for text analysis and its suspected relationship with the locations of ingredients in the store). 

The closest way to get a grocery list is a recipe, and we knew there were lots of recipes online.  This was still inspiring us to find ingredients on the shelf; wouldn’t it be cool to see how close you were to having a full recipe by inputting what you had in your kitchen?  But this still wasn’t fulfilling the requirement for text classification for a large corpus.  We decided we would just grab recipes and run classifications on them.  It made sense to the group because the context of text mining allowed us to grasp the concept of recipes as individual documents and the ingredients would be the text corpus.  Given what we learned in class, that a suitable corpus should have at least 10,000 documents for training models, it was suggested that we could come up with a text corpus by utilizing web scraping code based off a few R exercises we had done in class.  We suspected that we would possibly have luck using www.allrecipes.com after discovering their recipe URLs followed a numbering structure which would lend itself well to a loop function.  


#The Data Set

A little online research helped us stumble upon the Yummly API.  Another boon for the project was the additional discovery of the yummlyr package.  This cut down on our processing time because we were able to pull a list of recipe titles, the corresponding recipe ingredients, and the cuisine classifier for each recipe with a loop that called an API 26 times for 500 recipes each time instead of having a loop ping a website once each time for thousands of web pages.  Really easy:

recipes <- lapply(cuisinename, function (x) search_recipes(search_words="", allowed_cuisine=x, max_results = 500))

We noticed right away that we would need to save the recipe data instead of calling this API each time we wanted to work on our project, partly because the Academic API access for Yummly limited us to 30,000 calls for a lifetime, and secondly because the data that called for 500 maximum results for each of 26 calls would fail from time to time for reasons that are hard to pin down. If you lower the maximum results value, there is a better chance of the code running in R but we wanted to get as many as possible with as few API calls as possible. Perhaps depending on the network connection or the time of day you can consistently make this line of code work, but the problem required research removed from the text mining process of concern. 


#The Question 

Text mining is the practice of applying data mining on unstructured text data in order to learn something new.  What is something new we could learn from a large amount of recipe data?  We chose to move forward with classification of the recipes. One of the core things to understand about the classification problem was that we are trying to build text mining models to classify it for us, regardless of the provided classifier of “cuisine” from the Yummly API. Initially our interest was how similarities among ingredient lists would cluster recipes in a natural way, as opposed to using human defined cuisines as gold labels. A satisfying result would be the observation that said clusters could be easily named or summarized based upon the most frequent ingredients and Yummly cuisines in a cluster. This was a lesson learned, and not in the sense that there was any extensive cost of energy or time but in it’s purest form, the question: Why are we trying to classify recipe data with complicated models if it’s already classified (Yummly’s cuisine metadata) in the corpus?  The first attempt at running the code to pull in the recipe data only included the recipes and ingredients.  While running frequency functions on the code was easy enough, the task of clustering to create our own classifier was slow to start because we had an enormous amount of recipes with ingredients but couldn’t see if the clusters were making sense.  We knew we needed a data dictionary, but intuitively the data dictionary wasn’t very clear until we realized that there was ~4,000 unique ingredients and there was no way this could serve as a dictionary. Were we going to go through all those terms and come up with our own dictionary?  This would have proven a poor use of time considering that we could use the Yummly cuisines and in-class examples have shown us that these dictionaries are Phd-level projects, for instance this positive/negative word dictionary is cited as taking “years” to complete, and our project timeline was approximately 4 weeks.  

Using the text mining package in R, we would create a document term matrix of recipe and ingredients with the recipe name being an identifier.  It would follow that the frequency of each ingredient would serve as a feature with recipes as observations.

#Is It Creative?

While in the preprocessing phase, the question of classification was expanded to, “Could we come up with a better classifier than the cuisine types provided?”  As the commonly used idiom goes, ‘One man’s breakfast is another man’s dinner’.  What if a person was more interested in searching for a flavor rather than conventional or arbitrary search terms?  In group discussions, the following question was posed: “Does Mediterranean Pearled Couscous Salad sound Chinese to anyone?”  The possibility of misclassification could also lend itself to the usefulness of a model that would offer its own classification based on machine learning rather than human interpretation.

#Natural Language Processing and Text Processing Techniques

After we had compiled our corpus and set up a matrix showing us term frequency we took on a project of cleaning up some of the data. We had to make decisions on if there is a difference between stewed, diced, or peeled tomatoes significant enough to warrant the inclusion of these words. Diced, Kraft, sliced, boneless, skinless did not appear to be words that could imply any difference in the foodstuffs themselves which were ingredients, nor did it we believe that words like these should make a difference in a recipe categorization, so they were discarded. In the end there was a collection of 40 terms that we removed from the raw corpus, and another 60 terms that we manipulated so that the text mining package in R would not unnecessarily break apart multi-word ingredients into separate words.

#K Means Clustering

If one were to only look at the cuisine of a recipe, they may find hundreds of different flavor profiles within that cuisine. Imagine that you wanted mexican food and you were served a  sopaipillas, a delicious fried pastry popular in latin cultures. This dish would satisfy your request for mexican food, however when one craves mexican food they are often thinking of a taco, enchilada or meats cooked with spices and citrus flavors. Cuisine can be a good indicator of the likely flavors in a dish but the flavor of a recipe lies in the ingredients that make it up. Because of this, clustering serves as a way to categorize recipes based on their ingredients and in turn their flavor profiles. All assumptions of how cusine will impact the clusters must be ignored at first, here we are looking to make new “cuisines” that are based on the flavor. If one were to keep cuisines in the back of their mind while completing this analysis they may not be satisfied with a cluster that returns dishes often found in Central America and Thailand, when in reality those two regions share many of the same flavors in cilantro, citrus and spices.  

The process of clustering in R is relatively simple in theory, after pre-processing the corpus and converting the data into a Document-Term Matrix (DTM) it can be entered into the kmeans() command. In this analysis the DTM was Term Frequency - Inverse Document Frequency (TF-IDF) weighted. This was done to account for the fact that there were no stopwords to remove and while it seemed every ingredient was valuable to the data set there were ingredients like salt, pepper and olive oil that appeared in almost half of the recipes. The document matrix was transformed using the euclidean length formula. Because the unit vectors were normalized with the user defined norm_eucl function, the same seed or cluster centroid which was nearest to any particular observation by a euclidean metric would have been the same seed or centroid measured as nearest with the more common text mining metric of cosine distance. It would follow that the resulting clusters are the same under the implementation of either metric for any given seeds and subsequent centroids.

Initially, 26 clusters were used. There are 26 cuisines in the Yummly database so it seemed to be a natural starting point. Unfortunately, this did not return satisfactory results as the clusters varied greatly in size, and given the large number of clusters it was difficult to find interpretable difference between every cluster. After clustering many more times, it was decided that fifteen was the ideal number of clusters. This clustering returned twelve evenly sized clusters and two massive clusters. A second clustering was then performed on the two massive clusters. One returned strong results and the other failed to make logical sense. The cluster that returned strong results upon a second pass through kmeans() was appended to the data set as an additional five clusters. The re-clustering that returned another set of illogical groupings was put aside to be used as a data set for classifications via Naive Bayes. Also during this process many terms were found that were skewing the clustering process, these terms were modified by either dropping adjectives or combining common multi word ingredients like grated parmesan cheese. 
The output from K-means was used to create our new recipe classification groups that were based on our exploration from text-mining processes. The clusters were identified as “logical” based on their within sums of squares, term frequencies and cuisine frequencies. The cuisines were brought back into the data set because they acted as a strong indicator that the clustering outputs made sense.  

#Naive Bayes

We utilized a Naive Bayes model on the output of the K Means model in order to place new, unseen data into the originally created clusters. The question of interest was whether the recipe clusters created by the K Means model had distinctions recognizable by other modeling techniques. 
In the Document Term Matrix all values greater than 0 were converted to “Yes” and “No” values that were suitable for the Naive Bayes procedure. We used the clusters created by K Means as “gold labels” to train Naive Bayes. After removing the large “potluck” cluster and holding it for the model’s deployment, the Document Term Matrix containing members of the other clusters was partitioned into training and test sets. The misclassification rate on the test data was 10.5%. Two of the clusters created by K Means were much larger than the rest of the clusters, and one of these we have referred to here as a “potluck” because of its variety of cuisines and ingredients occurring in large numbers. We used the Naive Bayes model a second time to “re-cluster” these into the clusters created by the original Naive Bayes model. After viewing the members of that “potluck” in context with their newly assigned clusters, these redistributed subgroups generally were interpretable as having recipes of similar kinds and each was consistent with the outside clusters into which they were assigned. 
Class-by-class sensitivities (or cluster by cluster) with this model ranged from 45% to whereabouts of 99%. The model underperformed significantly in two difficult classes with 45% and 48% sensitivities. These appear significantly harder to classify correctly than others as the third poorest of 18 class sensitivities was 80%. All class specificities were around 99% or greater. This outcome would seem logical enough for classes that were not too diverse in their sizes. Even if a model was classifying recipes into one of 18 clusters at random, a false positive would only occur approximately 94% of the time on average. We know from our sensitivity values that the model performed far better than random guessing on our test data.  

#RTextTools

For our final model we decided we should use a decision tree, because hierarchical clustering would require more computing power than we possess. To do so we used the RTextTools package. In the package we were able to create the following models with minimal effort: Maximum Entropy,  Random Forest, Neural Network, Gradient Boosting, Bootstrap Aggregating, Support Vector Machine, Supervised Latent Dirichlet Allocation, as well as Decision Trees. We moved forward with all the models. 

Our gradient boosting, neural networks, and decision tree models severely underperformed. Boosting had 35% accuracy, neural networks had 3.5% and our decision tree had a accuracy of 8.6%.  Bagging, random forest, SVM, and SLDA all had accuracies within 3 percentage points of each other between 57% and 60%. The best overall performance was an ensemble model that used three of the eight models. Our ensemble model took 93% of the data and predicted the cuisine correctly 65% of the time. 
