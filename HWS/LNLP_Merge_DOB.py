
# coding: utf-8

# ## Import dataset

# In[1]:

import pandas as pd
import numpy as np
import datetime


# In[2]:

salef = pd.read_excel('H:\XIAOXI_JNJ\LakeNona LNLP\Data extract of participants in target zone\Python merge with age group\LNLP_31DEC2017_with_ID.xls')
ele = pd.read_excel('H:\XIAOXI_JNJ\LakeNona LNLP\Data extract of participants in target zone\Python merge with age group\Participants_Electronic_NoNull_Unique.xls')
paper = pd.read_csv('H:\XIAOXI_JNJ\LakeNona LNLP\Data extract of participants in target zone\Python merge with age group\stucked 4 together_paper_NoNull_Unique.csv')


# ## Overlook of datasets

# In[3]:

salef.head()


# In[4]:

salef=salef.rename(index=str,columns={"Consent Completed":"Consent_Completed",
                                     "User ID":"User_ID",
                                      "First Name":"First_Name",
                                      "Last Name":"Last_Name",
                                      "Community Name":"Community_Name"
                                     })


# In[5]:

salef.dtypes


# In[6]:

salef.describe(include='all')


# ## ---------------------------------------------------------------------------------------------------------

# In[7]:

ele.head()


# In[8]:

ele.dtypes


# ## ---------------------------------------------------------------------------------------------------------

# In[9]:

paper.head()


# In[10]:

paper.dtypes


# In[11]:

paper['ConsentCompleted']=pd.to_datetime(paper['ConsentCompleted'],errors='coerce') # NaT data include
paper['DOB']=pd.to_datetime(paper['DOB'],errors='coerce')
paper.head()


# In[12]:

paper.dtypes


# ## Electronic dataset Rename and dropping

# In[13]:

ele1=ele.rename(index=str, columns={"FirstName": "First_Name",
                                   "LastName": "Last_Name",
                                   "DOB_E":"DOB",
                                   "CommunityName":"Community_Name_E",
                                    "ID":"ID_E"})

ele2=ele1.drop(['Name', 'Paper_Electronic'],axis=1);  #axis means it's dropping columns not rows

ele2.head()


# In[14]:

ele2.describe(include='all')


# ## Paper dataset Rename and Dropping 

# In[15]:

paper1=paper.rename(index=str,columns={"FirstName":"First_Name",
                                     "LastName":"Last_Name",
                                     "ConsentCompleted":"Consent_Completed_P",
                                      "ID":"ID_P",
                                      "Label":"Label_P"})

paper2=paper1.drop(['Name','Include_or_not','Paper_Electronic'],axis=1)    #Put ‘;’ at the end of a line to surpress output
paper2.head()


# In[16]:

paper2.describe(include='all')


# ## Sorting

# In[17]:

salef_s=salef.sort_values(by=['First_Name','Last_Name'])
salef_s


# In[18]:

ele_s=ele2.sort_values(by=['First_Name','Last_Name'])
ele_s


# In[19]:

paper_s=paper2.sort_values(by=['First_Name','Last_Name'])
paper_s


# In[20]:

salef_dups=salef_s.duplicated(['First_Name','Last_Name'],keep='first')
salef_dups;


# In[21]:

salef_concat=pd.concat([salef_s, salef_dups], axis=1)  #stuck two data set together

salef_concat1=salef_concat.rename(index=str,columns = {0:'duplicate'}) #rename the column
salef_concat1.head()


# In[22]:

salef_concat1.sort_values(by='duplicate',ascending=True)
salef_concat1.describe(include="all")


# ## Conditional subset Dup and Non Dup

# In[23]:

dup=salef_concat1['duplicate']==1   #Bool type   
nondup=salef_concat1['duplicate']==0 
salef_dup=salef_concat1[dup]
salef_nondup=salef_concat1[nondup]
salef_dup


# In[24]:

salef_dup.info()


# In[25]:

salef_dup;


# In[26]:

salef_nondup.info()


# ## Merge Saleforce Paper and Electronic

# In[27]:

merge_se=pd.merge(salef_nondup,ele_s,on=['First_Name','Last_Name'], how='left')


# In[28]:

merge_se


# In[29]:

merge_se.info()


# In[30]:

merge_sep=pd.merge(merge_se,paper_s,on=['First_Name','Last_Name'], how='left')


# In[31]:

merge_sep


# In[32]:

merge_sep.info()


# ## Adding duplicate in saleforce

# In[33]:

merge_total=pd.concat([merge_sep,salef_dup])


# In[34]:

merge_total.describe(include='all')


# In[35]:

merge_total


# ## Let DOB conbine DOB from paper and electroinc 

# ### <font color='red'>Method 2:</font> Using  <font color='blue'>Where <font>to create conditional new column  <font color='BLUE'>(speed is lower than LOC) <font>
# 
# #### <font color='green'> merge_total['DOB'] = np.where (pd.isnull(merge_total['DOB_x']), merge_total.DOB_y, merge_total.DOB_x) </font>

# ### <font color='red'>Method 1: </font>Using  <font color='red'>df.loc[i,j] </font>      i is row, j is column  to subset conditional one

# In[36]:

merge_total["DOB"] =merge_total.DOB_x


# In[37]:

merge_total.loc[pd.isnull(merge_total['DOB_x']),'DOB'] = merge_total[pd.isnull(merge_total['DOB_x'])]['DOB_y']


# In[38]:

merge_total


# ## AGE CALCULATED BASED ON DOB

# In[39]:

now=np.datetime64(datetime.date.today())
now


# In[40]:

merge_total["today"] = np.datetime64(now)
merge_total["days"] = ""
merge_total["age"] = ""


# In[41]:

merge_total.days=(merge_total.today-merge_total.DOB)


# In[42]:

merge_total.age=round(merge_total.days.dt.days/365-0.5)    # Floor to the year


# In[43]:

merge_total


# In[44]:

merge_total.describe(include="all")


# ## Age group intervel

# In[45]:

df=merge_total
df["age_group"]=""


# In[46]:

df.loc[(df.age<19),'age_group'] = np.NaN
df.loc[(df.age>=19),'age_group'] = 1
df.loc[(df.age>=25),'age_group'] = 2
df.loc[(df.age>=30),'age_group'] = 3
df.loc[(df.age>=35),'age_group'] = 4
df.loc[(df.age>=40),'age_group'] = 5
df.loc[(df.age>=45),'age_group'] = 6
df.loc[(df.age>=50),'age_group'] = 7
df.loc[(df.age>=55),'age_group'] = 8
df.loc[(df.age>=60),'age_group'] = 9
df.loc[(df.age>=65),'age_group'] = 10
df.loc[(df.age>=70),'age_group'] = 11
df.loc[pd.isnull(df.age),'age_group'] = np.NaN
df.loc[(df.age<=0),'age_group'] = np.NaN
                                                      # using np.NaN as missing value


# In[47]:

pd.crosstab(index=df["age_group"],columns="count") 


# In[48]:

pd.crosstab(index=df["age"],columns="count")   # create frequency table


# In[49]:

df.sort_values(by=['DOB'],ascending=False)


# In[50]:

df


# In[51]:

df[['age_group','age']].info()


# In[52]:

df.describe(include="all")


# ## Final Data

# In[53]:

final=df.drop(['Community_Name_E','Consent_Completed_P','DOB_x','DOB_y','LabelE','Label_P','today','days','age'],axis=1)


# In[54]:

final.head()


# In[55]:

final_df=final[['User_ID','First_Name','Last_Name','Community_Name','Consent_Completed','DOB','age_group','duplicate']]


# In[56]:

final_df


# In[57]:

final_df.info()


# ## Target Zone

# In[58]:

final_target = final_df[(final_df['Community_Name']=='Laureate Park')|(final_df['Community_Name']=='Waters Edge')|(final_df['Community_Name']=='NorthLake Park' )]


# In[59]:

final_target.head()


# ## Export to cvs sep=','

# In[69]:

final_target.to_csv('H:/XIAOXI_JNJ/LakeNona LNLP/Data extract of participants in target zone/Python merge with age group/final_target_zone.csv',sep=',')


# In[68]:

final_df.to_csv('H:/XIAOXI_JNJ/LakeNona LNLP/Data extract of participants in target zone/Python merge with age group/final_all_zone.csv',sep=',')

