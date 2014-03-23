#prep and housekeeping
setwd("C:/DDM/Hw1/training_test_csv_files")
train=read.csv("training.csv",header=TRUE)
test=read.csv("test.csv",header=TRUE)

#paremeterized for Obesity, can be changed to any other disease
disease_ind=10596
total_words=10582

#problem 2.2 Function 1
contingency <- function (words, diseases)
{
	contingency_table <- table(diseases,words)
	return(contingency_table)
}

#problem 2.2 Function 2
termfreqdiff <- function(contingency_table)
{
	if(dim(contingency_table)[[2]]==2)
	{
		return((contingency_table[2,2]/(contingency_table[2,1]+contingency_table[2,2]))- contingency_table[1,2]/(contingency_table[1,1]+contingency_table[1,2]))
	}
	else if(dimnames(contingency_table)$words=="1")
	{
		return(1)
	}
	
	else
	{
		return(-1)	
	}
}

#problem 2.2 Function 3
infogain <-function(contingency_table)
{
	j=1
	infogain_val=0
	total=sum(contingency_table)
	while(j<=2)
	{
		if(dim(contingency_table)[[2]]==2)
		{
			k=1
			while(k<=2)
			{
				joint_prob=contingency_table[k,j]/total
				word_prob=sum(contingency_table[1:2,k])/total
				disease_prob=sum(contingency_table[j,1:2])/total
				if(joint_prob>0)
				{
					infogain_val=infogain_val+(joint_prob*log2(joint_prob/(word_prob*disease_prob)))
				}
				k=k+1
			}
			
		}
		else
		{
			joint_prob=contingency_table[j]/total
			infogain_val=infogain_val+joint_prob
			
		}
		j=j+1	
	}
	return(infogain_val)
}

#problem 2.2 Function 4
chisquare <-function(contingency_table)
{
	j=1
	chisquare_val=0
	total=sum(contingency_table)
	while(j<=2)
	{
		if(dim(contingency_table)[[2]]==2)
		{
			k=1
			while(k<=2)
				{
					joint_prob=contingency_table[k,j]/total
					word_prob=sum(contingency_table[1:2,k])/total
					disease_prob=sum(contingency_table[j,1:2])/total
					chisquare_val=chisquare_val+ ((joint_prob-(word_prob*disease_prob))^2/(word_prob*disease_prob))
					k=k+1
				}
		}
		else
		{
			joint_prob=contingency_table[j]/total
			chisquare_val=chisquare_val+joint_prob
		}
			j=j+1	
	}
	return(chisquare_val)
}


#logic to find the top terms using term frequency
word=NULL
vals=NULL
disease_list= c(10583, 10586, 10587, 10589, 10594, 10596, 10597)
iterator=7
termfreq1=data.frame(disease='',word='',freq=0)
while(iterator<=length(disease_list))
{
	count=1
	while(count<=total_words)
	{
		disease_iter=colnames(train)[disease_list[iterator]]
		val_iter=termfreqdiff(contingency(train[,count],train[,disease_list[iterator]]))
		word_iter=colnames(train)[count]
		tempfreq=data.frame(disease=disease_iter,word=word_iter,freq=val_iter)
		termfreq1=rbind(termfreq,tempfreq)
		count=count+1
	}
	iterator=iterator+1
}

#logic to find the top terms using infogain
word=NULL
vals=NULL
disease_list= c(10583, 10586, 10587, 10589, 10594, 10596, 10597)
iterator=1
infofreq=data.frame(disease='',word='',freq=0)
while(iterator<=length(disease_list))
{
	count=1
	while(count<=total_words)
	{
		disease_iter=colnames(train)[disease_list[iterator]]
		val_iter=infogain(contingency(train[,count],train[,disease_list[iterator]]))
		word_iter=colnames(train)[count]
		tempfreq=data.frame(disease=disease_iter,word=word_iter,freq=val_iter)
		infofreq=rbind(infofreq,tempfreq)
		count=count+1
	}
	iterator=iterator+1
}

#logic to find the top terms using chisquare
word=NULL
vals=NULL
disease_list= c(10583, 10586, 10587, 10589, 10594, 10596, 10597)
iterator=1
chifreq=data.frame(disease='',word='',freq=0)
while(iterator<=length(disease_list))
{
	count=1
	while(count<=total_words)
	{
		disease_iter=colnames(train)[disease_list[iterator]]
		val_iter=chisquare(contingency(train[,count],train[,disease_list[iterator]]))
		word_iter=colnames(train)[count]
		tempfreq=data.frame(disease=disease_iter,word=word_iter,freq=val_iter)
		chifreq=rbind(chifreq,tempfreq)
		count=count+1
	}
	iterator=iterator+1
}

#function to calculate the classification error given master and model data in 0/1
class_error <- function (master, model)
{
	iterator=1
	error_count=0
	while(iterator<=length(master))
	{
		if(master[iterator]!=model[iterator])
		{
			error_count=error_count+1
		}
		iterator=iterator+1
	}
	return(error_count/length(master))
}

#function to map Y/N to 0/1, used to calculate the classification error. Not optimized but works
class_boolean <- function (data)
{

	data_temp=data
	data_class=NULL
	iterator=1
	while(iterator<=length(data_temp))
	{
		if(data_temp[iterator]=="Y")
			data_class[iterator]=1
		else
			data_class[iterator]=0
		iterator=iterator+1
	}
	
	return(data_class)

}

#function to calculate standard validation measures
verify_measures <- function(master, predict)
{
	retrieved = sum(predict)
	precision = sum(predict & master)/sum(predict)
	print(precision)
	recall = sum(predict & master)/sum(master)
	print(recall)
	fmeasure = 2*precision*recall/(precision+recall)
	print(fmeasure)

}

#prior probabilities
prior_pos_prob=284/727
prior_neg_prob=443/727

#Get subset of document with disease=Y/N
disease_presence_set=subset(train, Obesity=='Y')
disease_absence_set=subset(train, Obesity=='N')

#logic to calculate the vocab size and total word occurences for both disease Y/N
total_count_disease_present=0
total_count_disease_absent=0
vocab_present_count=0
vocab_absent_count=0

iterator=1
while(iterator<=total_words)
{
	word_count_disease_present=0
	word_count_disease_absent=0
	
	#To be sure all 0/1 and Y/N combination are present
	if(dim(contingency(train[,iterator],train[,disease_ind]))[[2]]==2)
	{
		word_count_disease_present=contingency(train[,iterator],train[,disease_ind])[[2,2]]
		word_count_disease_absent=contingency(train[,iterator],train[,disease_ind])[[1,2]]
	}
	#if not, do all documents contain the word?
	else if(dimnames(contingency(train[,iterator],train[,disease_ind]))$words=="1")
	{
		word_count_disease_present=contingency(train[,iterator],train[,disease_ind])[[2,1]]
		word_count_disease_absent=contingency(train[,iterator],train[,disease_ind])[[1,1]]
	}
	#increment the vocab size for disease present
	if(word_count_disease_present>0)
	{
	
		total_count_disease_present=total_count_disease_present+word_count_disease_present
		vocab_present_count=vocab_present_count+1
	}
	#increment the vocab size for disease absent
	if(word_count_disease_absent>0)
	{
		total_count_disease_absent=total_count_disease_absent+word_count_disease_absent
		vocab_absent_count=vocab_absent_count+1
	}
	iterator=iterator+1
}

#Need laplace smoothening?
laplace_ind="Y"

#logic to calculate the probabilities for each word
if(laplace_ind=="Y")
{
	iterator=1
	prob_pos_word_data=NULL
	prob_neg_word_data=NULL


	while(iterator<=total_words)
	{
		positive_count=0
		negative_count=0

		prob_pos_word=0
		prob_neg_word=0

		if(length(unique(disease_presence_set[,iterator]))==2)
		{

			positive_count=table(disease_presence_set[,iterator])[[2]]
			prob_pos_word=(positive_count+1)/(total_count_disease_present+vocab_present_count)

		}
		else
		{
			if(unique(disease_presence_set[,iterator])==1)
			{
				prob_pos_word=(dim(disease_presence_set)[1]+1)/(total_count_disease_present+vocab_present_count)
			}
			else
			{
				prob_pos_word=1/(total_count_disease_present+vocab_present_count)
			}
		}

		if(length(unique(disease_absence_set[,iterator]))==2)
		{

			negative_count=table(disease_absence_set[,iterator])[[2]]
			prob_neg_word=(negative_count+1)/(total_count_disease_absent+vocab_absent_count)

		}
		else
		{
			if(unique(disease_absence_set[,iterator])==1)
			{
				prob_neg_word=(dim(disease_absence_set)[1]+1)/(total_count_disease_absent+vocab_absent_count)
			}
			else
			{
				prob_neg_word=1/(total_count_disease_absent+vocab_absent_count)
			}
		}

		prob_pos_word_data[iterator]=prob_pos_word
		prob_neg_word_data[iterator]=prob_neg_word

		iterator=iterator+1
	}
}
if(laplace_ind=="N")
{
	iterator=1
	prob_pos_word_data=NULL
	prob_neg_word_data=NULL

	while(iterator<=total_words)
	{
		positive_count=0
		negative_count=0

		prob_pos_word=0
		prob_neg_word=0

		if(length(unique(disease_presence_set[,iterator]))==2)
		{

			positive_count=table(disease_presence_set[,iterator])[[2]]
			prob_pos_word=(positive_count)/(total_count_disease_present)

		}
		else
		{
			if(unique(disease_presence_set[,iterator])==1)
			{
				prob_pos_word=(dim(disease_presence_set)[1])/(total_count_disease_present)
			}
			else
			{
				prob_pos_word=0
			}
		}

		if(length(unique(disease_absence_set[,iterator]))==2)
		{

			negative_count=table(disease_absence_set[,iterator])[[2]]
			prob_neg_word=(negative_count)/(total_count_disease_absent)

		}
		else
		{
			if(unique(disease_absence_set[,iterator])==1)
			{
				prob_neg_word=(dim(disease_absence_set)[1])/(total_count_disease_absent)
			}
			else
			{
				prob_neg_word=0
			}
		}

		prob_pos_word_data[iterator]=prob_pos_word
		prob_neg_word_data[iterator]=prob_neg_word

		iterator=iterator+1
	}
}

#function that applies NB probabilities and return the classified output
apply_nb <- function (data)
{
	doc_iterator=1
	disease_class=NULL
	pos_prob=NULL
	neg_prob=NULL
	while(doc_iterator<=nrow(data))
	{
		doc_pos_class_prob=0
		doc_neg_class_prob=0
		word_iterator=1
		while(word_iterator<=total_words)
		{
			if(data[doc_iterator,word_iterator]==1)
			{
				if(prob_pos_word_data[word_iterator]>0)
				{
					doc_pos_class_prob=doc_pos_class_prob+(data[doc_iterator,word_iterator]*log(prob_pos_word_data[word_iterator]))
				}

				if(prob_neg_word_data[word_iterator]>0)
				{
					doc_neg_class_prob=doc_neg_class_prob+(data[doc_iterator,word_iterator]*log(prob_neg_word_data[word_iterator]))
				}
			}
			word_iterator=word_iterator+1
		}

		norm_pos_prob=doc_pos_class_prob+log(prior_pos_prob)
		norm_neg_prob=doc_neg_class_prob+log(prior_neg_prob)

		pos_prob[doc_iterator]=norm_pos_prob
		neg_prob[doc_iterator]=norm_neg_prob

		if(norm_pos_prob>=norm_neg_prob)
		{
			disease_class[doc_iterator]='Y'
		}
		else
		{
			disease_class[doc_iterator]='N'
		}
		doc_iterator=doc_iterator+1
	}
	
	return(disease_class)
}

#call NB for training data set
disease_class_train=apply_nb(train)
write.csv(disease_class_train,"disease_class_train.csv")
nb_train_error=class_error(train[,disease_ind],disease_class_train)
nb_train_class=class_boolean(train[,disease_ind])
disease_boolean_train=class_boolean(disease_class_train)
verify_measures(nb_train_class,disease_boolean_train)

#call NB for test data set
disease_class_test=apply_nb(test)
write.csv(disease_class_test,"disease_class_test.csv")
nb_test_error=class_error(test[,disease_ind],disease_class_test)
nb_test_class=class_boolean(test[,disease_ind])
disease_boolean_test=class_boolean(disease_class_test)
verify_measures(nb_test_class,disease_boolean_test)

#for LR, SVM and RF, we only need selected features
train_shrink=train[,c(6503,6505,6013,8746,545,269,6517,10373,1611,1861,6014,308,1100,9097,10596)]
train_class=class_boolean(train_shrink[,15])

#for LR, SVM and RF, we only need selected features
test_shrink=test[,c(6503,6505,6013,8746,545,269,6517,10373,1611,1861,6014,308,1100,9097,10596)]
test_class=class_boolean(test_shrink[,15])

#Run LR
m=glm(Obesity~obese+obesity+morbid+sleep+apnea+albuterol+obstructive+wheezing+chronic+complications+morbidly+allergy+bowel+study, family=binomial, data=train_shrink)
summary(m)
out=as.factor(predict(m,train_shrink[,1:14])>0)
disease_class=NULL
iterator=1
while(iterator<=length(out))
{
	if(out[iterator]==TRUE)
	{
		disease_class[iterator]="Y"
	}
	else
	{
		disease_class[iterator]="N"
	}
	iterator=iterator+1
}

class_error(train_shrink[,15],disease_class)
disease_boolean=class_boolean(disease_class)
verify_measures(train_class,disease_boolean)


out=as.factor(predict(m,test_shrink[,1:14])>0)
disease_class=NULL
iterator=1
while(iterator<=length(out))
{
	if(out[iterator]==TRUE)
	{
		disease_class[iterator]="Y"
	}
	else
	{
		disease_class[iterator]="N"
	}
	iterator=iterator+1
}
class_error(test_shrink[,15],disease_class)
disease_boolean=class_boolean(disease_class)
verify_measures(test_class,disease_boolean)

#Run SVM
m=LiblineaR(train_shrink[,1:14],train_shrink[,15],type=1)
disease_class=predict(m,train_shrink[,1:14])
class_error(train_shrink[,15],disease_class$predictions)
disease_boolean=class_boolean(disease_class$predictions)
verify_measures(train_class,disease_boolean)


disease_class=predict(m,test_shrink[,1:14])
class_error(test_shrink[,15],disease_class$predictions)
disease_boolean=class_boolean(disease_class$predictions)
verify_measures(test_class,disease_boolean)

#Run RF
m=randomForest(train_shrink[,1:14],train_shrink[,15],do.Trace=T)
disease_class=predict(m)
class_error(train_shrink[,15],disease_class)
disease_boolean=class_boolean(disease_class)
verify_measures(train_class,disease_boolean)


disease_class=predict(m,test_shrink[,1:14])
class_error(test_shrink[,15],disease_class)
disease_boolean=class_boolean(disease_class)
verify_measures(test_class,disease_boolean)



