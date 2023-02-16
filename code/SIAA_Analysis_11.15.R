title: "SIAA_Analysis"
author: "Sameer Desai & Srividya Dasaraju"
date: "11/7/2020"
output:
  pdf_document: default
html_document: default

#check working directory
setwd("~/Documents/Documents - Srividya’s MacBook Air/SIAA")
getwd()

#load in data
SIAA_Data <- read.csv("SIAA_Raw_11-4-20_Revised.csv")

#load useful packages and libraries
library(readr)
library(ggplot2)
library(leaflet)
library(dplyr)
library(ggmap)
library(data.table)
library(Hmisc)
library(summarytools)
library(ggmap)
#load packages for encoding
#install.packages("encode")
library(encode)
#install.packages("forcats")
library(forcats)
#install.packages("cleandata")
library(cleandata)

## Demographics.
label(SIAA_Data$asian_background) <- "Do you consider any part of your background to be (Asian) Indian or Indian-American?"
label(SIAA_Data$participation_query) <- "Do you consent to participate in this study?"
label(SIAA_Data$country_residence) <- "What is your usual country of residence?"
label(SIAA_Data$respondent_type) <- "Are you a current college student or recent graduate, or a parent of a college student or recent graduate?"
label(SIAA_Data$matched_code) <- "Please enter the personal survey completion code in the email you received: "
label(SIAA_Data$US_born) <- "Were you born in the United States?"
label(SIAA_Data$india_born) <- "Were you born in India or elsewhere?"
label(SIAA_Data$misc_born) <- "Were you born in India or elsewhere? - Other country [specify]: "
label(SIAA_Data$religion) <- "What is your present religion, if any?"
label(SIAA_Data$caste_identification) <- "Do you personally identify with any caste?"
label(SIAA_Data$caste_classification) <- "Which of following caste categories do you identify with?"
label(SIAA_Data$gender) <- "What is your gender?"
label(SIAA_Data$year_of_birth) <- "In what year were you born?"
label(SIAA_Data$highest_education) <- "What is the highest level of education you have completed?"
label(SIAA_Data$current_employment) <- "Which of the following best describes your current employment status?"
label(SIAA_Data$family_income) <- "Thinking back over the last year, what was your family's annual income?"
label(SIAA_Data$state_of_residence) <- "What is your current state of residence?"
label(SIAA_Data$university_attended) <- "Which university in the United States do you currently attend/did you last attend?"
label(SIAA_Data$major_of_study) <- "What school does / did your major fall under? 
If you are or were a double major, select two options."
label(SIAA_Data$immigrant_status) <- "Which of these statements best describes you?"
label(SIAA_Data$year_immigrated) <- "In what year did you come to live in the United States?"
label(SIAA_Data$visa_status) <- "Are you currently on a green card or any type of visa?"
label(SIAA_Data$visa_status_misc) <- "Are you currently on a green card or any type of visa? - Other [specify]: "
label(SIAA_Data$citizenship_desire) <- "If given the opportunity, would you like to stay in the United States and become a U.S. citizen?"
label(SIAA_Data$citizenship_year) <- "In what year did you become a naturalized U.S. citizen?"
label(SIAA_Data$indian_citizen) <- "Are you an Indian citizen?"
label(SIAA_Data$visa_status) <- "Are you currently on a green card or any type of visa?"
label(SIAA_Data$visa_status_misc) <- "Are you currently on a green card or any type of visa? - Other [specify]: "
label(SIAA_Data$OCI_status) <- "Do you have an OCI (Overseas Citizenship of India) card?"
label(SIAA_Data$marital_status) <- "What is your marital status?"
label(SIAA_Data$spousal_origin) <- "Is your spouse or partner of Indian origin?"
label(SIAA_Data$spousal_birthplace) <- "Was your spouse or partner born in India?"		
label(SIAA_Data$father_birthplace) <- "What country was your father born in?"
label(SIAA_Data$father_birthplace_misc) <- "What country was your father born in? - Other country [specify]: "
label(SIAA_Data$mother_birthplace) <- "What country was your mother born in? "
label(SIAA_Data$mother_birthplace_misc) <- "What country was your mother born in? - Other country [specify]: "
label(SIAA_Data$US_education) <- "Did you complete all of your education in the United States?"
label(SIAA_Data$highest_external_education) <- "What is the highest level of education you completed outside of the United States?"
label(SIAA_Data$mother_language) <- "Other than English, what is the primary Indian language spoken by your mother?"
label(SIAA_Data$multiple_home_states) <- "Are there one or multiple states in India that you would call your home state(s)?"
label(SIAA_Data$india_home_state) <- "Which of these states in India would you call your home state?"
label(SIAA_Data$india_home_states) <- "Which of these states in India would you call your home states? Select all that apply."
label(SIAA_Data$family_india_residence) <- "Do any of your immediate family members (spouse, mother, father, brother, sister, son, daughter) live in India?"
## General US Politics.
label(SIAA_Data$political_leaning) <- "Generally speaking, do you think of yourself as a...?"
label(SIAA_Data$democrat_strength) <- "Would you call yourself a strong Democrat or a not very strong Democrat?"
label(SIAA_Data$closer_party) <- "Do you think of yourself as closer to the Democratic or the Republican Party?"
label(SIAA_Data$republican_strength) <- "Would you call yourself a strong Republican or a not very strong Republican?"
label(SIAA_Data$registered_party) <- "What political party are you registered with, if any?"
label(SIAA_Data$registered_party_misc) <- "What political party are you registered with, if any? - Other [specify]: "
label(SIAA_Data$vote_status) <- "Now thinking of the 2020 election, did you vote in a Presidential primary election or caucus this year?"
label(SIAA_Data$vote_choice) <- "In the Presidential primary or caucus in your state, who did you vote for?"
label(SIAA_Data$vote_choice_misc) <- "In the Presidential primary or caucus in your state, who did you vote for? - Other [specify]: "
label(SIAA_Data$president_choice) <- "Now we'd like to ask you about the election for President to be held on November 3, in which Joe Biden is running against Donald Trump. If you have already voted, whom did you vote for? If you have not voted, whom do you intend to vote for?"
label(SIAA_Data$president_choice_misc) <- "Now we'd like to ask you about the election for President to be held on November 3, in which Joe Biden is running against Donald Trump. If you have already voted, whom did you vote for? If you have not voted, whom do you intend to vote for? - Other [specify]: "
label(SIAA_Data$registered_party_misc) <- "What political party are you registered with, if any? - Other [specify]: "
label(SIAA_Data$pres_pref_strength) <- "Would you say your preference for this candidate is strong or not strong?"
label(SIAA_Data$candidate_choice) <- "Now we'd like to ask you about the election for President to be held on November 3, in which Joe Biden is running against Donald Trump. Which candidate do you support?"
label(SIAA_Data$candidate_choice_misc) <- "Now we'd like to ask you about the election for President to be held on November 3, in which Joe Biden is running against Donald Trump. Which candidate do you support? - Other [specify]: "
label(SIAA_Data$cand_pref_strength) <- "Would you say your preference for this candidate is strong or not strong?"
label(SIAA_Data$harris_voting) <- "You probably know that Democratic presidential nominee Joe Biden selected Kamala Harris as his vice-presidential candidate. Does the selection of Harris make you more or less likely to vote in the November 3 presidential election?"
label(SIAA_Data$harris_enthusiasm) <- "Does the selection of Kamala Harris as Joe Biden's vice presidential candidate make you more or less enthusiastic about Biden's candidacy?"
label(SIAA_Data$harris_displeasure) <- "Which of the following best describes why the selection of Kamala Harris as Joe Biden's vice presidential candidate makes you less enthusiastic about Biden's candidacy?"
label(SIAA_Data$harris_displeasure_misc) <- "Which of the following best describes why the selection of Kamala Harris as Joe Biden's vice presidential candidate makes you less enthusiastic about Biden's candidacy? - Other [specify]: "
label(SIAA_Data$harris_pleasure) <- "Which of the following best describes why the selection of Kamala Harris as Joe Biden's vice presidential candidate makes you more enthusiastic about Biden's candidacy?"
label(SIAA_Data$harris_pleasure_misc) <- "Which of the following best describes why the selection of Kamala Harris as Joe Biden‚Äôs vice presidential candidate makes you more enthusiastic about Biden's candidacy? - Other [specify]: "
label(SIAA_Data$political_activities) <- "In the last twelve months, have you participated in any of the following activities? Select all that apply."
label(SIAA_Data$media_importance) <- "How important is it that the media can report the news without state or government censorship?"
label(SIAA_Data$political_engagement) <- "Some people seem to follow what's going on in government and public affairs most of the time, whether there's an election going on or not. Others aren't that interested. Would you say you follow what's going on in government and public affairs in America ..."
label(SIAA_Data$vote_registration) <- "Are you registered to vote?"
label(SIAA_Data$prior_choice) <- "Who did you vote for in the election for President in 2016?"
label(SIAA_Data$prior_candidate) <- "Who did you support in the election for President in 2016?"
label(SIAA_Data$ideological_scale) <- "We hear a lot of talk these days about liberals and conservatives. Here is a seven-point scale on which the political views that people might hold are arranged from extremely liberal to extremely conservative. Where would you place yourself on this scale, or haven't you thought much about this?"
label(SIAA_Data$liberal_conservative) <- "If you had to choose, would you consider yourself a liberal or a conservative?"
label(SIAA_Data$US_landscape) <- "Do you feel things in the U.S. are generally going in the right direction, or do you feel things have pretty seriously gotten off on the wrong track?"
label(SIAA_Data$republican_immigration_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party is not supportive of legal immigration."
label(SIAA_Data$republican_economics_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party does not pursue my preferred economic policies."
label(SIAA_Data$republican_religious_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party is too influenced by Christian evangelicalism."
label(SIAA_Data$republican_discrim_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party is intolerant of minorities"
label(SIAA_Data$republican_india_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party is not good for India."
label(SIAA_Data$republican_guns_displeasure) <- "Which of the following best describes why you do NOT identify with the Republican Party? - The Republican Party opposes gun control."
label(SIAA_Data$largest_republican_difference) <- "On which economic policy issue, broadly defined, do your preferences differ the MOST from that of the Republican Party?"
label(SIAA_Data$largest_rep_diff_misc) <- "On which economic policy issue, broadly defined, do your preferences differ the MOST from that of the Republican Party? - Other [specify]: "
label(SIAA_Data$democrat_immigration_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party is weak on illegal immigration"
label(SIAA_Data$democratic_economics_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party does not pursue my preferred economic policies."
label(SIAA_Data$democrat_extremism_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party is too influenced by the extreme leftwing."
label(SIAA_Data$democrat_identity_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party is too focused on identity politics."
label(SIAA_Data$democrat_india_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party is not good for India."
label(SIAA_Data$democrat_crime_displeasure) <- "Which of the following best describes why you do NOT identify with the Democratic Party? - The Democratic Party is too soft on crime."
label(SIAA_Data$largest_democrat_difference) <- "On which economic policy issue, broadly defined, do your preferences differ the MOST from that of the Democratic Party?"
label(SIAA_Data$trump_approval) <- "Do you approve or disapprove of the way Donald Trump is handling his job as President?"
label(SIAA_Data$strength_trump_approval) <- "Do you approve/disapprove strongly or not strongly?"
label(SIAA_Data$trump_india_approval) <- "Do you approve or disapprove of the way Donald Trump is handling relations with India?"
label(SIAA_Data$strength_trump_india_approval) <- "Do you approve/disapprove strongly or not strongly?"
label(SIAA_Data$biden_cares) <- "Think about Joe Biden. In your opinion, does the phrase 'Joe Biden really cares about people like me' describe Joe Biden: "
label(SIAA_Data$biden_leads) <- "In your opinion, does the phrase 'Joe Biden provides strong leadership' describe Joe Biden: "
label(SIAA_Data$angered_biden) <- "How often would you say you've felt angry because of the kind of person Biden is or because of something Biden has done?"
label(SIAA_Data$afraid_biden) <- "How often would you say you've felt afraid because of the kind of person Biden is or because of something Biden has done?"
label(SIAA_Data$trump_cares) <- "Think about Donald Trump. In your opinion, does the phrase 'Donald Trump really cares about people like me' describe Donald Trump:"
label(SIAA_Data$trump_leads) <- "In your opinion, does the phrase 'Donald Trump provides strong leadership' describe Donald Trump:"
label(SIAA_Data$angered_trump) <- "How often would you say you've felt angry because of the kind of person Trump is or because of something Trump has done?"
label(SIAA_Data$afraid_trump) <- "How often would you say you've felt afraid because of the kind of person Trump is or because of something Trump has done?"
label(SIAA_Data$republican_feeling) <- "What are your feelings towards the persons or groups listed below? - Republican Party"
label(SIAA_Data$democrat_feeling) <- "What are your feelings towards the persons or groups listed below? - Democratic Party"
label(SIAA_Data$trump_feeling) <- "What are your feelings towards the persons or groups listed below? - Donald Trump"
label(SIAA_Data$harris_feeling) <- "What are your feelings towards the persons or groups listed below? - Kamala Harris"
label(SIAA_Data$biden_feeling) <- "What are your feelings towards the persons or groups listed below? - Joe Biden"
label(SIAA_Data$democrat_friends) <- "How comfortable are you having close friends that are Democrats?"
label(SIAA_Data$friend_democrat_married) <- "Suppose a friend of yours was getting married. How would you feel if he or she married a supporter of the Democratic Party? Would you be: "
label(SIAA_Data$republican_friends) <- "How comfortable are you having close friends that are Republicans?"
label(SIAA_Data$friend_republican_married) <- "Suppose a friend of yours was getting married. How would you feel if he or she married a supporter of the Republican Party? Would you be: "
label(SIAA_Data$democracy_leader) <- "Some feel that the United States should rely on a democratic form of government to solve its problems. Others feel that the United States should rely on a leader with a strong hand to solve its problems. Which comes closer to your opinion?"
label(SIAA_Data$china_opinion) <- "Which of the following best describes your opinion of China?"
label(SIAA_Data$china_response) <- "In recent years, the United States has formulated an Indo-Pacific strategy to counter China's growing influence. Do you think the United States should:"
label(SIAA_Data$news_source) <- "Which, if any, of the following have you used in the last week as a source of news about the United States?"
label(SIAA_Data$media_source) <- "Which, if any, of the following have you used in the last week for news about the United States?"
label(SIAA_Data$media_familiarity) <- "Which of the following media outlets are you familiar with as sources of news on American politics? Select all that apply."
label(SIAA_Data$nyt_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - The New York Times"
label(SIAA_Data$fox_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - Fox News"
label(SIAA_Data$npr_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - NPR"
label(SIAA_Data$wsj_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - WSJ"
label(SIAA_Data$msnbc_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - MSNBC"
label(SIAA_Data$cnn_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - CNN"
label(SIAA_Data$bbc_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - BBC"
label(SIAA_Data$govt_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - The national government"
label(SIAA_Data$social_media_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - Social Media (Facebook, Twitter)"
label(SIAA_Data$whatsapp_trust) <- "How trustworthy would you say news and information about politics and current affairs in the United States from the following sources is? - Messaging apps (WhatsApp)"
## Specific US Politics Issue Areas.
label(SIAA_Data$immigration_opinion) <- "Thinking about immigration, would you support or oppose providing undocumented immigrants a path to citizenship?"
label(SIAA_Data$affirmative_action) <- "Do you support or oppose the consideration of race or ethnic identity as a factor in university admissions to improve the representation of historically disadvantaged groups?"
label(SIAA_Data$religious_tolerance) <- "Do you support or oppose this statement: It is important for religious minorities to be treated the same way as the religious majority in a country."
label(SIAA_Data$police_brutality) <- "Do you support or oppose the use of force (such as tear gas, rubber bullets, physical force) by the police against peaceful protestors who are occupying public spaces (such as roads or highways)?"
label(SIAA_Data$media_suppression) <- "White House efforts to revoke press credentials of reporters who are critical of the Trump administration."
label(SIAA_Data$immigrant_deportation) <- "Enhanced efforts by ICE (Immigration and Customs Enforcement) officials to identify and deport illegal immigrants through border apprehensions and unannounced raids."
label(SIAA_Data$affirmative_action_2) <- "The consideration of the racial identity of applicants as a factor in U.S. university admissions to improve the representation of African-Americans."
label(SIAA_Data$travel_ban) <- "The 2017 presidential executive order to institute a travel ban for citizens from several, predominantly Muslim countries."
label(SIAA_Data$police_brutality_2) <- "The decision by police and law enforcement in some cities to use rubber bullets, tear gas and physical force against peaceful Black Lives Matter protesters who are occupying public spaces (such as roads or highways)."
label(SIAA_Data$white_supremacy_threat) <- "Some people say white supremacy is a threat to minorities in the American democracy. Do you agree?"
label(SIAA_Data$economy_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Economy"
label(SIAA_Data$education_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Education"
label(SIAA_Data$environment_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Environment / climate change"
label(SIAA_Data$inequality_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Income inequality"
label(SIAA_Data$immigration_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Immigration"
label(SIAA_Data$racism_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Racism / racial discrimination"
label(SIAA_Data$sexism_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Sexism / gender discrimination"
label(SIAA_Data$tax_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Taxes"
label(SIAA_Data$US_india_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - US-India relations"
label(SIAA_Data$healthcare_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Healthcare"
label(SIAA_Data$terrorism_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Terrorism"
label(SIAA_Data$corruption_issue) <- "As you think about the issues influencing your vote choice in this November's presidential election, which of the following are the top 3 most important issues for you personally? - Government corruption"
## General India Politics.
label(SIAA_Data$better_US_India_relations) <- "When it comes to managing India-US relations, which party do you think does a better job?"
label(SIAA_Data$US_India_importance) <- "How important will a candidate's position on India be for you personally in deciding for whom you will vote this November?"
label(SIAA_Data$US_India_relationship) <- "Now thinking about the relationship between the United States and India...Is the U.S. "
label(SIAA_Data$india_domestic_division) <- "Do you think that domestic politics in India are dividing the Indian-American community in the United States?"
label(SIAA_Data$india_division_issues) <- "What features of India's domestic politics are most responsible for creating divisions in the Indian-American community? Select all that apply"
label(SIAA_Data$india_division_issues_misc) <- "What features of India‚Äôs domestic politics are most responsible for creating divisions in the Indian-American community? - Other [specify]: "
label(SIAA_Data$community_relations) <- "Do you agree or disagree with the following statement: Overall, the Indian American community has a positive impact on U.S.-India relations."
label(SIAA_Data$india_activities) <- "Have you engaged in any of the following activities? Select all that apply."
label(SIAA_Data$india_culture) <- "Have you engaged in any of the following activities? Select all that apply."
label(SIAA_Data$india_connection) <- "How connected do you personally feel with India?"
label(SIAA_Data$india_feeling) <- "Which of the following best describes you?"
label(SIAA_Data$india_identity) <- "How important is being Indian to your identity?"
label(SIAA_Data$US_identity) <- "Which of the following best describes your identity?"
label(SIAA_Data$US_Asian_identity) <- "Which of the following would you say best describes your background?"
label(SIAA_Data$US_Asian_identity_misc) <- "Which of the following would you say best describes your background? - Other [specify]: "
label(SIAA_Data$indian_org) <- "Are you a member of any Indian-American organization or group? This includes cultural, regional, religious, caste, community, or school-based organizations."
label(SIAA_Data$india_celebrations) <- "Do you participate in celebrations of any of the following? Select all that apply."
label(SIAA_Data$friend_group) <- "Which of the following best describes your personal group of friends?"
label(SIAA_Data$friend_religions) <- "Amongst your Indian friends, how many are from the same religion as you?"
label(SIAA_Data$friend_caste) <- "Amongst your Indian friends, how many are from the same caste as you?"
label(SIAA_Data$friend_region) <- "Amongst your Indian friends, how many are from the same region (in India) as you?"
label(SIAA_Data$indianamerican_discrim) <- "In general, do you think discrimination against people of Indian origin in the United States is a: "
label(SIAA_Data$indianamerican_discrim_comparative) <- "In your opinion, do people discriminate against Indian-Americans in the United States MORE THAN any of the following groups? Select all that apply: "
label(SIAA_Data$personal_discrimination) <- "In the last 12 months, have you personally felt discriminated against because of any of the following reasons? Select all that apply."
label(SIAA_Data$skin_color_discr) <- "Who discriminated against you on the basis of the color of your skin?"
label(SIAA_Data$religious_discr) <- "Who discriminated against you on the basis of religion?"
label(SIAA_Data$gender_discr) <- "Who discriminated against you on the basis of gender?"
label(SIAA_Data$caste_discr) <- "Who discriminated against you on the basis of caste?"
label(SIAA_Data$birth_discr) <- "Who discriminated against you on the basis of your country of origin?"
label(SIAA_Data$india_political_engagement) <- "Would you say you follow what's going on in government and public affairs in India: "
label(SIAA_Data$india_landscape) <- "Do you feel things in India are generally going in the right direction, or do you feel things have pretty seriously gotten off on the wrong track?"
label(SIAA_Data$india_political_party) <- "Which political party in India do you identify with the most?"
label(SIAA_Data$india_party_misc) <- "Which political party in India do you identify with the most? - Other [specify]: "
label(SIAA_Data$modi_approval) <- "Do you approve or disapprove of the way Narendra Modi is handling his job as Prime Minister?"
label(SIAA_Data$modi_approval_strength) <- "Do you approve/disapprove strongly or not strongly?"
label(SIAA_Data$modi_cares) <- "In your opinion, does the phrase 'Narendra Modi really cares about people like me' describe Modi: "
label(SIAA_Data$modi_leader) <- "In your opinion, does the phrase 'Narendra Modi provides strong leadership' describe Modi: "
label(SIAA_Data$angered_modi) <- "How often would you say you've felt angry because of the kind of person Modi is or because of something Modi has done?"
label(SIAA_Data$afraid_modi) <- "How often would you say you've felt afraid because of the kind of person Modi is or because of something Modi has done?"
label(SIAA_Data$india_familiarity) <- "Which of these Indian leaders or organizations are you aware of? Select all that apply."
label(SIAA_Data$congress_party_feeling) <- "What are your feelings towards the person or group listed below? - Congress Party"
label(SIAA_Data$bjp_feeling) <- "What are your feelings towards the person or group listed below? - BJP (Bharatiya Janta Party)"
label(SIAA_Data$modi_feeling) <- "What are your feelings towards the person or group listed below? - Narendra Modi"
label(SIAA_Data$rahul_gandhi_feeling) <- "What are your feelings towards the person or group listed below? - Rahul Gandhi"
label(SIAA_Data$rss_feeling) <- "What are your feelings towards the person or group listed below? - RSS (Rashtriya Swayamsevak Sangh)"
label(SIAA_Data$bjp_friend) <- "How comfortable are you having close friends that are BJP supporters?"
label(SIAA_Data$congress_party_friend) <- "How comfortable are you having close friends that are Congress supporters?"
label(SIAA_Data$hindu_friends) <- "How comfortable are you having close friends that are Hindu?"
label(SIAA_Data$hindu_friend_marry) <- "Suppose a friend of yours was getting married. How would you feel if he or she married a Hindu? Would you be - "
label(SIAA_Data$muslim_friend) <- "How comfortable are you having close friends that are Muslim?"
label(SIAA_Data$muslim_friend_marry) <- "Suppose a friend of yours was getting married. How would you feel if he or she married a Muslim? Would you be - "
label(SIAA_Data$democracy_leader_india) <- "Some feel that India should rely on a democratic form of government to solve its problems. Others feel that India should rely on a leader with a strong hand to solve its problems. Which comes closer to your opinion?"
label(SIAA_Data$news_source_india) <- "Which, if any, of the following have you used in the last week as a source of news about India?"
label(SIAA_Data$media_source_india) <- "Which, if any, of the following have you used in the last week for news about India?"
label(SIAA_Data$media_familiarity_india) <- "Are you familiar with the following media outlets as sources of news on Indian politics? Select all that apply."
label(SIAA_Data$nyt_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - The New York Times"
label(SIAA_Data$bbc_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - BBC"
label(SIAA_Data$cnn_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - CNN"
label(SIAA_Data$economist_trust) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - The Economist"
label(SIAA_Data$toi_trust) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - Times of India"
label(SIAA_Data$ndtv_trust) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - NDTV"
label(SIAA_Data$republic_tv_trust) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - Republic TV"
label(SIAA_Data$aaj_tak_trust) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - Aaj Tak"
label(SIAA_Data$govt_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - The national government"
label(SIAA_Data$social_media_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - Social media (Facebook, Twitter)"
label(SIAA_Data$whatsapp_trust_india) <- "How trustworthy would you say news and information about politics and current affairs in India from the following sources is? - Messaging apps (WhatsApp)"
label(SIAA_Data$religious_importance) <- "How important is religion in your life?"
label(SIAA_Data$religious_services) <- "Aside from wedding and funerals, how often do you attend religious services?"
label(SIAA_Data$praying_frequency) <- "Outside of attending religious services, how often do you pray?"
## Specific India Politics Issue Areas.
label(SIAA_Data$media_suppression_india) <- "Government efforts to use defamation and sedition laws to silence reporters critical of the Modi administration."
label(SIAA_Data$immigration_deportation_india) <- "The proposal for an all-India National Register of Citizens (NRC) to document all legal citizens of India so that illegal migrants can be identified and deported."
label(SIAA_Data$citizenship_india) <- "The passage of the Citizenship Amendment Act 2019, which creates an expedited path to citizenship for migrants from neighboring countries who illegally entered India by 2014, provided they belong to non-Muslim religions."
label(SIAA_Data$affirmative_action_india) <- "The consideration of the caste identity of applicants as a factor in Indian university admissions to improve the representation of Dalits/Scheduled Castes."
label(SIAA_Data$police_brutality_india) <- "The decision by police and law enforcement in some cities to use rubber bullets, tear gas and physical force against peaceful protesters opposing recent citizenship laws who are occupying public spaces (such as roads or highways)."
label(SIAA_Data$hindutwa_threat) <- "In India, some people say Hindu majoritarianism is a threat to minorities in Indian democracy. Do you agree?"
label(SIAA_Data$economy_issue_india) <- "Which of the following are the top three challenges facing India today? - Economy"
label(SIAA_Data$education_issue_india) <- "Which of the following are the top three challenges facing India today? - Education"
label(SIAA_Data$environment_issue_india) <- "Which of the following are the top three challenges facing India today? - Environment"
label(SIAA_Data$healthcare_issue_india) <- "Which of the following are the top three challenges facing India today? - Healthcare"
label(SIAA_Data$inequality_issue_india) <- "Which of the following are the top three challenges facing India today? - Inequality"
label(SIAA_Data$caste_issue_india) <- "Which of the following are the top three challenges facing India today? - Caste Discrimination"
label(SIAA_Data$religious_issue_india) <- "Which of the following are the top three challenges facing India today? - Religious Majoritarianism"
label(SIAA_Data$genderdiscr_issue_india) <- "Which of the following are the top three challenges facing India today? - Gender Discrimination"
label(SIAA_Data$terrorism_issue_india) <- "Which of the following are the top three challenges facing India today? - Terrorism"
label(SIAA_Data$china_issue_india) <- "Which of the following are the top three challenges facing India today? - China"



# First, we filter the dataframe to only have parent responses.
parent_responses <- SIAA_Data %>%
  dplyr::filter(SIAA_Data$respondent_type == "Parent of current college student or recent graduate")
# Next, we filter the dataframe to only have student responses.
student_responses <- SIAA_Data %>%
  dplyr::filter(SIAA_Data$respondent_type == "Current college student or recent graduate")
# Adding an identifier tag to our parent responses.
colnames(parent_responses) <- paste(colnames(parent_responses), "parents", sep = "_")
parent_responses <- parent_responses %>% 
  rename(matched_code = matched_code_parents)
# Adding an identifier tag to our student responses.
colnames(student_responses) <- paste(colnames(student_responses), "students", sep = "_")
student_responses <- student_responses[-c(15)]
student_responses <- student_responses %>% 
  rename(matched_code = mturkcode_students)
# Now, we want to merge matched codes to mturk codes using our above two dataframes.
matched_SIAA <- left_join(student_responses, parent_responses, by=c("matched_code"), all.x=TRUE)
matched_SIAA <- matched_SIAA %>%
  dplyr::filter(!is.na(matched_SIAA$start_date_parents))



# Now, I subset the matched data to just student responses.
matched_student <- matched_SIAA %>% select(1:241)
# Here I do the same for parent responses.
matched_parent <- matched_SIAA %>% select(242:482)
# Comparing all values in Student and Parent dataframes, and returning a boolean.
SP_comparison <- as.data.frame(matched_student == matched_parent)
# Changing boolean to binary format.
SP_comparisonA <- as.data.frame(1 * (matched_student == matched_parent)) 
SP_comparisonA[is.na(SP_comparisonA)] <- 0
# Removing "_student" suffix, and adding "_binary" suffix.
colnames(SP_comparisonA) <- gsub('_students','',colnames(SP_comparisonA))
colnames(SP_comparisonA) <- paste(colnames(SP_comparisonA), "binary", sep = "_")

# Rejoin with matched_SIAA. 
matched_SIAA <- cbind(matched_SIAA, SP_comparisonA)

write.csv(matched_SIAA,"matched_siaa.csv", row.names = TRUE)

###### EXPLANATION OF DATA PROCESS FROM NOW ########

#In the next two chunks of code, I'm first encoding all of our categorical variables into numeric values (i.e. assigning "strongly agree" with a value of 4, and "strongly oppose" with a value of 0). Next, I am using these newly generated encodings to take our absolute differences across columns.

#Here's what's left to be done.

#In Step 1 (Encoding): We have a range of different ordinal variables in the survey, each of which have different answer sets. For example, some might ask if the respondent "strongly agrees" (or an associated scale) with a statement. Others might ask if the respondent thinks a policy is "very important" (or an associated scale). Others still might ask how the respondent fits on an ideological spectrum (i.e. from very liberal to very conservative). 
#The process for each of these question sets is the same; and is provided below (the code is the same from Q1 to Q8). However, for each chunk of code, we have to specify the unique "levels" (or answer ranges) of the relevant variables. 
#So far, I've done this for a bunch of variables, including those which ask about agreement; importance; frequency; etc. You'll see the indices and levels of the variables already assigned below.
#However, there are still more ordinal variables in the survey who haven't yet been encoded, because I haven't copied the code and relabeled their levels. This is the last step in the process. You'll simply copy the code from below, change the variable levels, and assign them to the relevant column indices, until no ordinal variables are left to encode. That's all that's left for Step 1.
#Note, the levels start by assigning the smallest values first, then increase. So for Q1, the first level I list "strongly oppose" takes a value of 1. The last level, "strongly support", takes a value of 4. Keep this in mind when labelling the levels, and order them logically (so the weakest agreement should be first, for example).


# In Step 2 (Absolute Differences): Step 1 is the tedious part. After that, life becomes easier. 
# All you need to do in Step 2, is revise the filters (column indices) which generate the student and parent dataframes to reflect the new ordinal variables you've added. 
# If you run the code first and look at the student and parent datasets I've generated, you'll see what I mean. Once you have added the new column indices into the filters, just run the code as usual and everything will compile.
# After that, we only have analysis left!


# Reading in the matched data.
matched_SIAA <- read_csv("matched_siaa.csv")
# Encoding across column indices for each question type. First, I filter the dataframe to only include responses of a certain categorical type (i.e. support/oppose vs. all the time/never).
Q1 <- matched_SIAA[c(72:75, 110:114, 195:199, 313:316, 351:355, 436:440)]
# Then, I define the column indices as a list.
cols <- c(1:28)
# Next, I use the lapply function to convert each column to factors, and specify the levels.
Q1[cols] <- lapply(Q1[cols], factor, levels=c("Strongly oppose", "Somewhat oppose", "Somewhat support", "Strongly support"))
# Lastly, I use the sapply function to convert the ordered factors to numerics, and revert to a dataframe.
Q1 <- sapply(Q1, as.numeric)
Q1 <- as.data.frame(Q1)
# Now, I add an encoded tag to our variable names.
colnames(Q1) <- paste(colnames(Q1), "encoded", sep = "_")
# I also create a unique row variable to merge back to our initial dataframe.
Q1 <- tibble::rowid_to_column(Q1, "X1")
matched_SIAA <- merge(matched_SIAA, Q1, by = c("X1"), all=TRUE)
# Now, we repeat the process for other question types.
Q2 <- matched_SIAA[c(71, 312)]
cols <- c(1:2)
Q2[cols] <- lapply(Q2[cols], factor, levels=c("Not at all important", "Slightly important", "Moderately important", "Very important"))
Q2 <- sapply(Q2, as.numeric)
Q2 <- as.data.frame(Q2)
colnames(Q2) <- paste(colnames(Q2), "encoded", sep = "_")
Q2 <- tibble::rowid_to_column(Q2, "X1")
matched_SIAA <- merge(matched_SIAA, Q2, by = c("X1"), all=TRUE)
# Now, we repeat the process for other question types.
Q3 <- matched_SIAA[c(138, 239, 379, 480)]
cols <- c(1:4)
Q3[cols] <- lapply(Q3[cols], factor, levels=c("Not at all important", "Not too important", "Somewhat important", "Very important", "One of the most important"))
Q3 <- sapply(Q3, as.numeric)
Q3 <- as.data.frame(Q3)
colnames(Q3) <- paste(colnames(Q3), "encoded", sep = "_")
Q3 <- tibble::rowid_to_column(Q3, "X1")
matched_SIAA <- merge(matched_SIAA, Q3, by = c("X1"), all=TRUE)
Q4 <- matched_SIAA[c(76, 185, 317, 426)]
cols <- c(1:4)
Q4[cols] <- lapply(Q4[cols], factor, levels=c("Hardly at all", "Only now and then", "Some of the time", "Most of the time"))
Q4 <- sapply(Q4, as.numeric)
Q4 <- as.data.frame(Q4)
colnames(Q4) <- paste(colnames(Q4), "encoded", sep = "_")
Q4 <- tibble::rowid_to_column(Q4, "X1")
matched_SIAA <- merge(matched_SIAA, Q4, by = c("X1"), all=TRUE)
Q5 <- matched_SIAA[c(80, 321)]
cols <- c(1:2)
Q5[cols] <- lapply(Q5[cols], factor, levels=c("Haven't thought much about this", "Extremely conservative", "Conservative", "Slightly conservative", "Moderate; middle of the road", "Slightly liberal", "Liberal", "Extremely liberal"))
Q5 <- sapply(Q5, as.numeric)
Q5 <- as.data.frame(Q5)
colnames(Q5) <- paste(colnames(Q5), "encoded", sep = "_")
Q5 <- tibble::rowid_to_column(Q5, "X1")
matched_SIAA <- merge(matched_SIAA, Q5, by = c("X1"), all=TRUE)
Q6 <- matched_SIAA[c(102:103, 106:107, 191:192, 343:344, 347:348, 432:433)]
cols <- c(1:12)
Q6[cols] <- lapply(Q6[cols], factor, levels=c("Don't know", "Not well at all", "Slightly well", "Moderately well", "Very well", "Extremely well"))
Q6 <- sapply(Q6, as.numeric)
Q6 <- as.data.frame(Q6)
colnames(Q6) <- paste(colnames(Q6), "encoded", sep = "_")
Q6 <- tibble::rowid_to_column(Q6, "X1")
matched_SIAA <- merge(matched_SIAA, Q6, by = c("X1"), all=TRUE)
Q7 <- matched_SIAA[c(104:105, 193:194, 345:346)]
cols <- c(1:6)
Q7[cols] <- lapply(Q7[cols], factor, levels=c("Don't know", "Never", "Some of the time", "About half the time", "Most of the time", "Always"))
Q7 <- sapply(Q7, as.numeric)
Q7 <- as.data.frame(Q7)
colnames(Q7) <- paste(colnames(Q7), "encoded", sep = "_")
Q7 <- tibble::rowid_to_column(Q7, "X1")
matched_SIAA <- merge(matched_SIAA, Q7, by = c("X1"), all=TRUE)
Q8 <- matched_SIAA[c(108:109, 349:350)]
cols <- c(1:4)
Q8[cols] <- lapply(Q8[cols], factor, levels=c("Don't know", "Never", "Sometimes", "About half the time", "Most of the time", "Always"))
Q8 <- sapply(Q8, as.numeric)
Q8 <- as.data.frame(Q8)
colnames(Q8) <- paste(colnames(Q8), "encoded", sep = "_")
Q8 <- tibble::rowid_to_column(Q8, "X1")
matched_SIAA <- merge(matched_SIAA, Q8, by = c("X1"), all=TRUE)
# Continue on, using same code as above.... remember to rename dataframe to next integer each time.
#############################
match("pres_pref_strength_students",names(matched_SIAA))
match("pres_pref_strength_parents",names(matched_SIAA))
match("cand_pref_strength_students",names(matched_SIAA))
match("cand_pref_strength_parents",names(matched_SIAA))
Q9 <- matched_SIAA[c(60, 63, 301, 304)]
cols <- c(1:4)
Q9[cols] <- lapply(Q9[cols], factor, levels=c("Not strong", "Strong"))
Q9 <- sapply(Q9, as.numeric)
Q9 <- as.data.frame(Q9)
colnames(Q9) <- paste(colnames(Q9), "encoded", sep = "_")
Q9 <- tibble::rowid_to_column(Q9, "X1")
matched_SIAA <- merge(matched_SIAA, Q9, by = c("X1"), all=TRUE)



match("modi_approval_strength_students",names(matched_SIAA))
match("modi_approval_strength_parents",names(matched_SIAA))
match("strength_trump_approval_students",names(matched_SIAA))
match("strength_trump_approval_parents",names(matched_SIAA))
match("strength_trump_india_approval_students",names(matched_SIAA))
match("strength_trump_india_approval_parents",names(matched_SIAA))
Q10 <- matched_SIAA[c(99, 101, 190, 340, 342, 431)]
cols <- c(1:6)
Q10[cols] <- lapply(Q10[cols], factor, levels=c("Not strongly","Strongly"))
Q10 <- sapply(Q10, as.numeric)
Q10 <- as.data.frame(Q10)
colnames(Q10) <- paste(colnames(Q10), "encoded", sep = "_")
Q10 <- tibble::rowid_to_column(Q10, "X1")
matched_SIAA <- merge(matched_SIAA, Q10, by = c("X1"), all=TRUE)


match("white_supremacy_threat_students",names(matched_SIAA))
match("white_supremacy_threat_parents",names(matched_SIAA))
match("community_relations_students",names(matched_SIAA))
match("community_relations_parents",names(matched_SIAA))
match("hindutwa_threat_students",names(matched_SIAA))
match("hindutwa_threat_parents",names(matched_SIAA))
Q11 <- matched_SIAA[c(115, 146, 200, 356, 387, 441)]
cols <- c(1:6)
Q11[cols] <- lapply(Q11[cols], factor, levels=c("Don't know", "Strongly disagree", "Somewhat disagree", 
                                                "Somewhat agree", "Strongly agree"))
Q11 <- sapply(Q11, as.numeric)
Q11 <- as.data.frame(Q11)
colnames(Q11) <- paste(colnames(Q11), "encoded", sep = "_")
Q11 <- tibble::rowid_to_column(Q11, "X1")
matched_SIAA <- merge(matched_SIAA, Q11, by = c("X1"), all=TRUE)

match("US_landscape_parents",names(matched_SIAA))
match("US_landscape_students",names(matched_SIAA))
match("india_landscape_students",names(matched_SIAA))
match("india_landscape_parents",names(matched_SIAA))
Q12 <- matched_SIAA[c(82, 186, 323, 427)]
cols <- c(1:4)
Q12[cols] <- lapply(Q12[cols], factor, levels=c("Wrong track", "Right track"))
Q12 <- sapply(Q12, as.numeric)
Q12 <- as.data.frame(Q12)
colnames(Q12) <- paste(colnames(Q12), "encoded", sep = "_")
Q12 <- tibble::rowid_to_column(Q12, "X1")
matched_SIAA <- merge(matched_SIAA, Q12, by = c("X1"), all=TRUE)



match("trump_approval_students",names(matched_SIAA))
match("trump_approval_parents",names(matched_SIAA))
match("modi_approval_students",names(matched_SIAA))
match("modi_approval_parents",names(matched_SIAA))
match("trump_india_approval_parents",names(matched_SIAA))
match("trump_india_approval_students",names(matched_SIAA))
Q13 <- matched_SIAA[c(98, 100, 189, 341, 339, 430)]
cols <- c(1:6)
Q13[cols] <- lapply(Q13[cols], factor, levels=c("Disapprove", "Approve"))
Q13 <- sapply(Q13, as.numeric)
Q13 <- as.data.frame(Q13)
colnames(Q13) <- paste(colnames(Q13), "encoded", sep = "_")
Q13 <- tibble::rowid_to_column(Q13, "X1")
matched_SIAA <- merge(matched_SIAA, Q13, by = c("X1"), all=TRUE)

match("friend_religions_students",names(matched_SIAA))
match("friend_religions_parents",names(matched_SIAA))
match("friend_caste_students",names(matched_SIAA))
match("friend_caste_parents",names(matched_SIAA))
match("friend_region_students",names(matched_SIAA))
match("friend_region_parents",names(matched_SIAA))
Q14 <- matched_SIAA[c(174:176, 415:417)]
cols <- c(1:6)
Q14[cols] <- lapply(Q14[cols], factor, levels=c("None of them", "Hardly any of them", "Some of them",
                                                "Most of them", "All of them"))
Q14 <- sapply(Q14, as.numeric)
Q14 <- as.data.frame(Q14)
colnames(Q14) <- paste(colnames(Q14), "encoded", sep = "_")
Q14 <- tibble::rowid_to_column(Q14, "X1")
matched_SIAA <- merge(matched_SIAA, Q14, by = c("X1"), all=TRUE)


match("bjp_friend_students",names(matched_SIAA))
match("bjp_friend_parents",names(matched_SIAA))
match("congress_party_friend_students",names(matched_SIAA))
match("congress_party_friend_parents",names(matched_SIAA))
match("hindu_friends_students",names(matched_SIAA))
match("hindu_friends_parents",names(matched_SIAA))
match("democrat_friends_students",names(matched_SIAA))
match("democrat_friends_parents",names(matched_SIAA))
match("muslim_friend_students",names(matched_SIAA))
match("muslim_friend_parents",names(matched_SIAA))
Q15 <- matched_SIAA[c(207:209, 448:450, 121, 362, 211, 452)]
cols <- c(1:10)
Q15[cols] <- lapply(Q15[cols], factor, levels=c("Not comfortable", "Somewhat comfortable", "Very comfortable"))
Q15 <- sapply(Q15, as.numeric)
Q15 <- as.data.frame(Q15)
colnames(Q15) <- paste(colnames(Q15), "encoded", sep = "_")
Q15 <- tibble::rowid_to_column(Q15, "X1")
matched_SIAA <- merge(matched_SIAA, Q15, by = c("X1"), all=TRUE)


match("religious_services_students",names(matched_SIAA))
match("religious_services_parents",names(matched_SIAA))
match("praying_frequency_students",names(matched_SIAA))
match("praying_frequency_parents",names(matched_SIAA))
Q16 <- matched_SIAA[c(240:241, 481:482)]
cols <- c(1:4)
Q16[cols] <- lapply(Q16[cols], factor, levels=c("Never", "Seldom", "A few times a year", "Once or twice a month", 
"Once a week", "More than once a week"))
Q16 <- sapply(Q16, as.numeric)
Q16 <- as.data.frame(Q16)
colnames(Q16) <- paste(colnames(Q16), "encoded", sep = "_")
Q16 <- tibble::rowid_to_column(Q16, "X1")
matched_SIAA <- merge(matched_SIAA, Q16, by = c("X1"), all=TRUE)


match("friend_democrat_married_students",names(matched_SIAA))
match("friend_democrat_married_parents",names(matched_SIAA))
match("friend_republican_married_students",names(matched_SIAA))
match("friend_republican_married_parents",names(matched_SIAA))
match("hindu_friend_marry_students",names(matched_SIAA))
match("hindu_friend_marry_parents",names(matched_SIAA))
match("muslim_friend_marry_students",names(matched_SIAA))
match("muslim_friend_marry_parents",names(matched_SIAA))
Q17 <- matched_SIAA[c(122, 124, 210, 212, 363, 365, 451,  453)]
cols <- c(1:8)
Q17[cols] <- lapply(Q17[cols], factor, levels=c("Not at all upset", "Somewhat upset", "Very upset"))
Q17 <- sapply(Q17, as.numeric)
Q17 <- as.data.frame(Q17)
colnames(Q17) <- paste(colnames(Q17), "encoded", sep = "_")
Q17 <- tibble::rowid_to_column(Q17, "X1")
matched_SIAA <- merge(matched_SIAA, Q17, by = c("X1"), all=TRUE)


match("india_identity_students",names(matched_SIAA))
match("india_identity_parents",names(matched_SIAA))
Q18 <- matched_SIAA[c(167, 408)]
cols <- c(1:2)
Q18[cols] <- lapply(Q18[cols], factor, levels=c("Somewhat unimportant", "Somewhat important", "Very important"))
Q18 <- sapply(Q18, as.numeric)
Q18 <- as.data.frame(Q18)
colnames(Q18) <- paste(colnames(Q18), "encoded", sep = "_")
Q18 <- tibble::rowid_to_column(Q18, "X1")
matched_SIAA <- merge(matched_SIAA, Q18, by = c("X1"), all=TRUE)

match("democrat_strength_students",names(matched_SIAA))
match("democrat_strength_parents",names(matched_SIAA))
Q19 <- matched_SIAA[c(50, 291)]
cols <- c(1:2)
Q19[cols] <- lapply(Q19[cols], factor, levels=c("Not very strong Democrat", "Strong Democrat"))
Q19 <- sapply(Q19, as.numeric)
Q19 <- as.data.frame(Q19)
colnames(Q19) <- paste(colnames(Q19), "encoded", sep = "_")
Q19 <- tibble::rowid_to_column(Q19, "X1")
matched_SIAA <- merge(matched_SIAA, Q19, by = c("X1"), all=TRUE)


match("republican_strength_students",names(matched_SIAA))
match("republican_strength_parents",names(matched_SIAA))
Q20 <- matched_SIAA[c(52, 293)]
cols <- c(1:2)
Q20[cols] <- lapply(Q20[cols], factor, levels=c("Not very strong Republican", "Strong Republican"))
Q20 <- sapply(Q20, as.numeric)
Q20 <- as.data.frame(Q20)
colnames(Q20) <- paste(colnames(Q20), "encoded", sep = "_")
Q20 <- tibble::rowid_to_column(Q20, "X1")
matched_SIAA <- merge(matched_SIAA, Q20, by = c("X1"), all=TRUE)

#not sure if this one is ordinal...?
match("US_identity_students",names(matched_SIAA))
match("US_identity_parents",names(matched_SIAA))
Q21 <- matched_SIAA[c(168, 409)]
cols <- c(1:2)
Q21[cols] <- lapply(Q21[cols], factor, levels=c("I feel neither Indian nor American", "I feel more Indian than American",
                                               "I feel equally Indian and American", "I feel more American than Indian"))
Q21 <- sapply(Q21, as.numeric)
Q21 <- as.data.frame(Q21)
colnames(Q21) <- paste(colnames(Q21), "encoded", sep = "_")
Q21 <- tibble::rowid_to_column(Q21, "X1")
matched_SIAA <- merge(matched_SIAA, Q21, by = c("X1"), all=TRUE)

match("US_India_relationship_students",names(matched_SIAA))
match("US_India_relationship_parents",names(matched_SIAA))
Q22 <- matched_SIAA[c(139, 380)]
cols <- c(1:2)
Q22[cols] <- lapply(Q22[cols], factor, levels=c("Don't know", "Not supportive enough of India",
                                                "U.S. support for India is about right", "Too supportive of India"))
Q22 <- sapply(Q22, as.numeric)
Q22 <- as.data.frame(Q22)
colnames(Q22) <- paste(colnames(Q22), "encoded", sep = "_")
Q22 <- tibble::rowid_to_column(Q22, "X1")
matched_SIAA <- merge(matched_SIAA, Q22, by = c("X1"), all=TRUE)


match("harris_voting_students",names(matched_SIAA))
match("harris_voting_parents",names(matched_SIAA))
Q23 <- matched_SIAA[c(64, 305)]
cols <- c(1:2)
Q23[cols] <- lapply(Q23[cols], factor, levels=c("Makes no difference", "Less likely", "More likely"))
Q23 <- sapply(Q23, as.numeric)
Q23 <- as.data.frame(Q23)
colnames(Q23) <- paste(colnames(Q23), "encoded", sep = "_")
Q23 <- tibble::rowid_to_column(Q23, "X1")
matched_SIAA <- merge(matched_SIAA, Q23, by = c("X1"), all=TRUE)


match("harris_enthusiasm_students",names(matched_SIAA))
match("harris_enthusiasm_parents",names(matched_SIAA))
Q24 <- matched_SIAA[c(65, 306)]
cols <- c(1:2)
Q24[cols] <- lapply(Q24[cols], factor, levels=c("Makes no difference", "Less enthusiastic","More enthusiastic"))
Q24 <- sapply(Q24, as.numeric)
Q24 <- as.data.frame(Q24)
colnames(Q24) <- paste(colnames(Q24), "encoded", sep = "_")
Q24 <- tibble::rowid_to_column(Q24, "X1")
matched_SIAA <- merge(matched_SIAA, Q24, by = c("X1"), all=TRUE)


match("india_connection_students",names(matched_SIAA))
match("india_connection_parents",names(matched_SIAA))
Q25 <- matched_SIAA[c(165, 406)]
cols <- c(1:2)
Q25[cols] <- lapply(Q25[cols], factor, levels=c("Not too connected", "Somewhat connected", "Very connected", "Extremely connected")) 
Q25 <- sapply(Q25, as.numeric)
Q25 <- as.data.frame(Q25)
colnames(Q25) <- paste(colnames(Q25), "encoded", sep = "_")
Q25 <- tibble::rowid_to_column(Q25, "X1")
matched_SIAA <- merge(matched_SIAA, Q25, by = c("X1"), all=TRUE)


match("china_opinion_students",names(matched_SIAA))
match("china_opinion_parents",names(matched_SIAA))
Q26 <- matched_SIAA[c(141, 382)]
cols <- c(1:2)
Q26[cols] <- lapply(Q26[cols], factor, levels=c("Very unfavorable", "Somewhat unfavorable", "Somewhat favorable", "Very favorable")) 
Q26 <- sapply(Q26, as.numeric)
Q26 <- as.data.frame(Q26)
colnames(Q26) <- paste(colnames(Q26), "encoded", sep = "_")
Q26 <- tibble::rowid_to_column(Q26, "X1")
matched_SIAA <- merge(matched_SIAA, Q26, by = c("X1"), all=TRUE)


match("india_feeling_students",names(matched_SIAA))
match("india_feeling_parents",names(matched_SIAA))
Q27<- matched_SIAA[c(166, 407)]
cols <- c(1:2)
Q27[cols] <- lapply(Q27[cols], factor, levels=c("Generally not pro-India","Generally pro-India but also critical of some of the Indian government's policies", 
                                                "Generally pro-India but also critical of many of the Indian government's policies",  "Generally pro-India and supportive of the Indian government's policies")) 
Q27 <- sapply(Q27, as.numeric)
Q27 <- as.data.frame(Q27)
colnames(Q27) <- paste(colnames(Q27), "encoded", sep = "_")
Q27 <- tibble::rowid_to_column(Q27, "X1")
matched_SIAA <- merge(matched_SIAA, Q27, by = c("X1"), all=TRUE)


match("friend_group_students",names(matched_SIAA))
match("friend_group_parents",names(matched_SIAA))
Q28<- matched_SIAA[c(173, 414)]
cols <- c(1:2)
Q28[cols] <- lapply(Q28[cols], factor, levels=c("Very few of my friends are of Indian origin", "Some of my friends are Indian", 
"Most of my friends are of Indian origin", "All my friends are of Indian origin")) 
Q28 <- sapply(Q28, as.numeric)
Q28 <- as.data.frame(Q28)
colnames(Q28) <- paste(colnames(Q28), "encoded", sep = "_")
Q28 <- tibble::rowid_to_column(Q28, "X1")
matched_SIAA <- merge(matched_SIAA, Q28, by = c("X1"), all=TRUE)


match("indianamerican_discrim_students",names(matched_SIAA))
match("indianamerican_discrim_parents",names(matched_SIAA))
Q29<- matched_SIAA[c(177, 418)]
cols <- c(1:2)
Q29[cols] <- lapply(Q29[cols], factor, levels=c("Not a problem", "Minor problem", "Major problem")) 
Q29 <- sapply(Q29, as.numeric)
Q29 <- as.data.frame(Q29)
colnames(Q29) <- paste(colnames(Q29), "encoded", sep = "_")
Q29 <- tibble::rowid_to_column(Q29, "X1")
matched_SIAA <- merge(matched_SIAA, Q29, by = c("X1"), all=TRUE)

match("liberal_conservative_students",names(matched_SIAA))
match("liberal_conservative_parents",names(matched_SIAA))
Q30<- matched_SIAA[c(81, 322)]
cols <- c(1:2)
Q30[cols] <- lapply(Q30[cols], factor, levels=c("Liberal", "Moderate", "Conservative")) 
Q30 <- sapply(Q30, as.numeric)
Q30 <- as.data.frame(Q30)
colnames(Q30) <- paste(colnames(Q30), "encoded", sep = "_")
Q30 <- tibble::rowid_to_column(Q30, "X1")
matched_SIAA <- merge(matched_SIAA, Q30, by = c("X1"), all=TRUE)






##########################################
# Removing irrelevant dataframes.
keep(matched_SIAA, sure = TRUE)
# First, split the dataset to just have student encodings and parent encodings (as Sri did above).
students <- matched_SIAA[c(725:738)]
parents <- matched_SIAA[c(739:752)]
# Then, drop the suffixes "_student" and "_parent" from each of those datasets.
colnames(students) <- gsub('_students','',colnames(students))
colnames(parents) <- gsub('_parents','',colnames(parents))
# Lastly, define our for loop to iterate over column names.
# For each column in the split data:
for (i in colnames(students)){
# Generate a new column in the original data which takes the difference of each column.
  absdiff <- paste(i, "diff", sep = "_", collapse = NULL)
  matched_SIAA[absdiff] <- abs(students[i] - parents[i])
} 


{r visualizations, echo=FALSE, message = FALSE}
# Generating initial tabulations and summary tables.
Hmisc::describe(SIAA_Data)
skim(SIAA_Data)
saved_x11_option <- st_options("use.x11")
st_options(use.x11 = TRUE)
dfSummary(SIAA_Data, plain.ascii = FALSE, style = "grid", 
          graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "/tmp")
view(dfSummary(SIAA_Data))





