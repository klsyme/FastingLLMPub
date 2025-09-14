library(readxl)
library(dplyr)
library(stringr)
d0 <- read_excel("Datasets/Full Unresolved Dataset.xlsx")
subset <- read_excel("Datasets/Reconciled subset copy.xlsx")


# 'd0' is the original data frame and 'subset' is the subset of d0
# Both data frames have an 'ID' column

# Create 'd' by removing rows from 'd0' where the 'ID' is also present in 'subset' and remove ID's 292, 310, 318
d <- d0 %>%
  anti_join(subset, by = "ID") %>%
  filter(!(ID %in% c(292, 310, 318)))
# 'd' will now be the dataset where rows present in 'subset' are removed from 'd0'

# Rename content of vectors

table(d$cognitive_1.x)
table(d$cognitive_1.y)
table(d$cognitive_2.x)
table(d$cognitive_2.y)
table(d$cognitive_3.x)
table(d$cognitive_3.y)
#table(d$cognitive_4.x)
#table(d$cognitive_4.y)

table(d$costs_1.x)
table(d$costs_1.y)
table(d$costs_2.x)
table(d$costs_2.y)
table(d$costs_3.x)
table(d$costs_3.y)
#table(d$costs_4.x)
#table(d$costs_4.y)

table(d$signal_type.x)
table(d$signal_type.y)

table(d$Gender.x)
table(d$Gender.y)

table(d$`time lapse.x`)
table(d$`time lapse.y`)




# Coder 1 Rename for Cognitive States: Create the new column cog1.x based on the conditions
d <- d %>%
  mutate(cog1.x = case_when(
    cognitive_1.x %in% c('meditation', 'contemplation', 'trance', 'altered state of consciousness', 'contemplative stupor', 'dissociation', 'ecstatic state', 'lose consciousness', 'possession state', '‚Äúmind is not weighted down with food') ~ 'asc', # Assign 'asc' for 'meditation' and 'trance'
    cognitive_1.x %in% c('knowledge', 'dream', 'dreams', 'hallucinatory', 'high state of spiritual consciousness', 'learn things', 'prophecy or soothsaying', 'spiritual vision', 'purifies mind', 'spiritual vision', 'vision', 'visionary', 'wisdom') ~ 'vk', # Assign 'vk' for 'knowledge' and 'see the things of heaven'
    cognitive_1.x %in% c('ghost will get him', 'visited by spirits') ~ 'sb', # Assign 'sb' for 'talk with ghosts' (written for multiple matches)
    cognitive_1.x %in% c('prepared to die willingly') ~ 'sc', # Assign 'sc' for 'self-control'
    cognitive_1.x %in% c('NA') ~ '0',
    TRUE ~ cognitive_1.x # Keep the original value if it doesn't match any of the conditions
  ))

table(d$cog1.x)

# Coder 2 Rename for Cognitive States: Create the new column cog1.y based on the conditions

d <- d %>%
  mutate(cog1.y = case_when(
    cognitive_1.y %in% c('alters mentality', 'ASC', 'concentration', 'peace', 'dissociation', 'peaceful?', 'trance') ~ 'asc', # Assign 'asc' for 'meditation' and 'trance'
    cognitive_1.y %in% c('knowledge', 'dream', 'dreams', 'visions', 'elevated consciousness', 'learn things', 'vision', 'spiritual knowledge', 'power', 'powers', 'learning', 'insight', 'hallucinations') ~ 'vk', # Assign 'vk' for 'knowledge' and 'see the things of heaven'
    cognitive_1.y %in% c('mental fortitude', 'strong will', 'endurance') ~ 'sc', # Assign 'sc' for 'self-control'
    cognitive_1.y %in% c('NA', 'drunk', 'cannot control cravings') ~ '0',
    TRUE ~ cognitive_1.y # Keep the original value if it doesn't match any of the conditions
  ))

# Coder 1 Rename for Cognitive States: Create the new column cog2.x based on the conditions
d <- d %>%
  mutate(cog2.x = case_when(
    cognitive_2.x %in% c('excitations', 'lose consciousness', 'trance') ~ 'asc', 
    cognitive_2.x %in% c('knowledge', 'dream', 'dreams', 'hallucinatory', 'specialized knowledge of power', 'learn things', 'thoughts', 'clear visions', 'clear thoughts', 'visions', 'learn', 'esoteric knowledge', 'hallucination', 'spiritual vision', 'purifies mind', 'vision', 'visionary', 'wisdom', 'idea') ~ 'vk', 
    cognitive_2.x %in% c('ghost will get him', 'visited by spirits') ~ 'sb', 
    cognitive_2.x %in% c('prepared to die willingly') ~ 'sc', 
    cognitive_2.x == 'NA' ~ '0',  # Convert string 'NA' to '0'
    is.na(cognitive_2.x) ~ '0', # Handle missing (NA) values
    TRUE ~ cognitive_2.x # Keep original value for non-matches
  ))

table(d$cog2.x)

# Coder 2 Rename for Cognitive States: Create the new column cog2.y based on the conditions

d <- d %>%
  mutate(cog2.y = case_when(
    cognitive_2.y %in% c('alters mentality', 'ASC', 'concentration', 'peace', 'dissociation', 'peaceful?', 'trance') ~ 'asc', # Assign 'asc' for 'meditation' and 'trance'
    cognitive_2.y %in% c('knowledge', 'insight', 'dream', 'dreams', 'visions', 'elevated consciousness', 'learn things', 'vision', 'spiritual knowledge', 'power', 'powers', 'learning', 'insight', 'hallucinations') ~ 'vk', # Assign 'vk' for 'knowledge' and 'see the things of heaven'
    cognitive_2.y %in% c('mental fortitude', 'strong will', 'endurance') ~ 'sc', # Assign 'sc' for 'self-control'
    cognitive_2.y %in% c('NA', 'cannot control cravings', 'self-esteem', 'self-control') ~ '0', #Put self-esteem for now for purposes of LLM paper
    TRUE ~ cognitive_2.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$cog2.y)

# Coder 1 Rename for Cognitive States: Create the new column cog3.x based on the conditions
d <- d %>%
  mutate(cog3.x = case_when(
    cognitive_3.x %in% c('excitations', 'lose consciousness', 'trance', 'self-imposed hypnotism') ~ 'asc', 
    cognitive_3.x %in% c('knowledge', 'knowledge of things in earth and heaven', 'plans of life', 'dream', 'dreams', 'hallucinatory', 'specialized knowledge of power', 'learn things', 'thoughts', 'clear visions', 'clear thoughts', 'visions', 'learn', 'esoteric knowledge', 'hallucination', 'spiritual vision', 'purifies mind', 'vision', 'visionary', 'wisdom', 'idea') ~ 'vk', 
    cognitive_3.x %in% c('ghost will get him', 'visited by spirits') ~ 'sb', 
    cognitive_3.x %in% c('prepared to die willingly') ~ 'sc', 
    cognitive_3.x == 'NA' ~ '0',  # Convert string 'NA' to '0'
    is.na(cognitive_3.x) ~ '0', # Handle missing (NA) values
    TRUE ~ cognitive_3.x # Keep original value for non-matches
  ))

table(d$cog3.x)

# Coder 2 Rename for Cognitive States: Create the new column cog2.y based on the conditions

d <- d %>%
  mutate(cog3.y = case_when(
    cognitive_3.y %in% c('alters mentality', 'ASC', 'concentration', 'peace', 'dissociation', 'peaceful?', 'trance') ~ 'asc', # Assign 'asc' for 'meditation' and 'trance'
    cognitive_3.y %in% c('knowledge', 'naming knowledge', 'insight', 'dream', 'dreams', 'visions', 'elevated consciousness', 'learn things', 'vision', 'spiritual knowledge', 'power', 'powers', 'learning', 'insight', 'hallucinations') ~ 'vk', # Assign 'vk' for 'knowledge' and 'see the things of heaven'
    cognitive_3.y %in% c('mental fortitude', 'strong will', 'endurance') ~ 'sc', # Assign 'sc' for 'self-control'
    cognitive_3.y %in% c('NA', 'cannot control cravings', 'self-esteem', 'self-control') ~ '0', #Put self-esteem for now for purposes of LLM paper
    TRUE ~ cognitive_3.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$cog3.y)

# Coder 1 Rename for Costs: Create the new column cos1.x based on the conditions
d <- d %>%
  mutate(cos1.x = case_when(
    costs_1.x %in% c('weak', 'stagger', 'suffer', 'nausea', 'insomnia', 'thirst', 'ugly', 'starvation', 'unable to walk', 'faint away', 'faint', 'exhaustion', 'body depressed', 'death-like state', 'difficulty', 'loss of sleep', 'exhausted', 'strenuous', 'pale', 'emaciated', 'throats so parched could barely speak', 'lose consciousness', 'did not live long', 'faint', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_1.x %in% c('mating', 'restrict timing of marriages', 'sex') ~ 'mat',
    costs_1.x %in% c('financial/resource', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_1.x %in% c('vulnerable to attack', 'quarrels and fights', 'cannot offer food to guests', 'fasting too much is selfish', 'die if break fast', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_1.x %in% c('isolation', 'lost in the bush', 'seclusion', 'sleep outside') ~ 'sw', 
    costs_1.x %in% c('NA', 'flagellation', 'bled', 'scrape tongue with knife', 'swallow stick', 'take poison', 'torture', 'turned into a partridge', 'vomiting blood', 'whipping', 'self-mortification', 'sit for 4 days', 'sitting at door', 'sick from drinking tobacco juice', 'self-torture', 'penance', 'risk of death for faster or someone in their family if they accept blessing from snake during vision', 'responsibilities to spirits', 'burden', 'mourning', 'mortification', 'burns on face', 'castigation', 'cripples from foresweating spirits who visisted them during fast', 'curse from serpent and children and grandchildren contract tuberculosis', 'fingers torn from learning to play fiddle', 'flings self headfirst at wall', 'forced vomiting', 'leap from cliff', 'self-harm', 'special food taboo', 'scratch body with rock', 'loss of impulse control', 'bleeding', 'hemorrhaging from nose and mouth', 'wash in urine?', 'torment', 'scarpe body with stone', 'emesis', 'pilgrimage', 'nose hemorrhage', 'breaks lobes of ears', 'danger', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_1.x # Keep the original value if it doesn't match any of the conditions
  ))

table(d$cos1.x)


# Coder 2 Rename for Costs: Create the new column cos1.y based on the conditions
d <- d %>%
  mutate(cos1.y = case_when(
    costs_1.y %in% c('weak', 'illness from greedy request', 'illness if fast during wrong time period', 'loss of voice', 'energy?', 'mental disorder', 'mind depressed', 'crying', 'unconscious', 'faint from hunger and thirst', 'sickness if fast too young', 'health if old', 'vomiting', 'vomiting?', 'strenuous', 'tuberculosis', 'stagger', 'seizure', 'thinness', 'sick', 'pale', 'lost consciousness', 'hypoglycemia', 'haggard', 'illness from greedy request illness if fast during wrong time period', 'hunger', 'lean', 'lots of effort', 'empty stomach', 'difficult for elderly', 'energy', 'discomfort', 'avoiding favorite foods (lard & cheese)', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_1.y %in% c('sexual', 'no courting/dating', 'mating', 'restrict timing of marriages', 'sex', 'reduced reproductive success - fidelity') ~ 'mat',
    costs_1.y %in% c('financial/resource', 'millet to poor', 'alms-giving', 'money', 'helps the poor', 'blood money', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_1.y %in% c('vulnerable to attack', 'death of society', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_1.y %in% c('isolation', 'lost in the bush', 'seclusion') ~ 'sw', 
    costs_1.y %in% c('NA', 'less self control', 'rearranges schedule', 'strict rules', 'turned into a partridge', 'strict', 'give up meat', 'beaten', 'blood-letting?', 'also had to dip hand in boiling water', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_1.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$costs_1.y)
table(d$cos1.y)

# Coder 1 Rename for Costs: Create the new column cos2.x based on the conditions

d <- d %>%
  mutate(cos2.x = case_when(
    costs_2.x %in% c('weak', 'loss of sleep', 'exhausted', 'strenuous', 'pale', 'emaciated', 'throats so parched could barely speak', 'lose consciousness', 'did not live long', 'faint', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_2.x %in% c('mating', 'restrict timing of marriages', 'sex') ~ 'mat',
    costs_2.x %in% c('financial/resource', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_2.x %in% c('vulnerable to attack', 'cannot offer food to guests', 'fasting too much is selfish', 'die if break fast', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_2.x %in% c('isolation', 'lost in the bush', 'seclusion', 'sleep outside') ~ 'sw', 
    costs_2.x %in% c('NA', 'hemorrhaging from nose and mouth', 'wash in urine?', 'torment', 'scarpe body with stone', 'emesis', 'pilgrimage', 'nose hemorrhage', 'breaks lobes of ears', 'danger', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_2.x # Keep the original value if it doesn't match any of the conditions
  ))

table(d$costs_2.x)
table(d$cos2.x)

# Coder 2 Rename for Costs: Create the new column cos2.y based on the conditions

d <- d %>%
  mutate(cos2.y = case_when(
    costs_2.y %in% c('weak', 'thirst', 'ugly', 'starvation', 'unable to walk', 'faint away', 'exhaustion', 'body depressed', 'death-like state', 'difficulty', 'loss of sleep', 'exhausted', 'strenuous', 'pale', 'emaciated', 'throats so parched could barely speak', 'lose consciousness', 'did not live long', 'faint', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_2.y %in% c('mating', 'restrict timing of marriages', 'sex') ~ 'mat',
    costs_2.y %in% c('financial/resource', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_2.y %in% c('vulnerable to attack', 'cannot offer food to guests', 'fasting too much is selfish', 'die if break fast', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_2.y %in% c('isolation', 'lost in the bush', 'seclusion', 'sleep outside') ~ 'sw', 
    costs_2.y %in% c('NA', 'loss of impulse control', 'bleeding', 'hemorrhaging from nose and mouth', 'wash in urine?', 'torment', 'scarpe body with stone', 'emesis', 'pilgrimage', 'nose hemorrhage', 'breaks lobes of ears', 'danger', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_2.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$costs_2.y)
table(d$cos2.y)

# Coder 1 Rename for Costs: Create the new column cos3.x based on the conditions

d <- d %>%
  mutate(cos3.x = case_when(
    costs_3.x %in% c('weak', 'suffer', 'insomnia', 'thirst', 'ugly', 'starvation', 'unable to walk', 'faint away', 'faint', 'exhaustion', 'body depressed', 'death-like state', 'difficulty', 'loss of sleep', 'exhausted', 'strenuous', 'pale', 'emaciated', 'throats so parched could barely speak', 'lose consciousness', 'did not live long', 'faint', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_3.x %in% c('mating', 'restrict timing of marriages', 'sex') ~ 'mat',
    costs_3.x %in% c('financial/resource', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_3.x %in% c('vulnerable to attack', 'quarrels and fights', 'cannot offer food to guests', 'fasting too much is selfish', 'die if break fast', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_3.x %in% c('isolation', 'lost in the bush', 'seclusion', 'sleep outside') ~ 'sw', 
    costs_3.x %in% c('NA', 'self-harm', 'special food taboo', 'scratch body with rock', 'loss of impulse control', 'bleeding', 'hemorrhaging from nose and mouth', 'wash in urine?', 'torment', 'scarpe body with stone', 'emesis', 'pilgrimage', 'nose hemorrhage', 'breaks lobes of ears', 'danger', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_3.x # Keep the original value if it doesn't match any of the conditions
  ))

table(d$costs_3.x)
table(d$cos3.x)

# Coder 2 Rename for Costs: Create the new column cos3.y based on the conditions

d <- d %>%
  mutate(cos3.y = case_when(
    costs_3.y %in% c('weak', 'suffer', 'insomnia', 'thirst', 'ugly', 'starvation', 'unable to walk', 'faint away', 'faint', 'exhaustion', 'body depressed', 'death-like state', 'difficulty', 'loss of sleep', 'exhausted', 'strenuous', 'pale', 'emaciated', 'throats so parched could barely speak', 'lose consciousness', 'did not live long', 'faint', 'weakness', 'avoid water on hot day', 'abstain from pleasures and joys', 'worsening health', 'hunger', 'suffering', 'thin', 'thirsty', 'toil on empty stomach', 'very hard for old women', 'very thin', 'ugly and haggard', 'suffer', 'state of hunger', 'starving', 'a vision at too young an age could cause illness', 'contemplation', 'angry indignations', 'arduous', 'barely able to walk', 'become worn out', 'body and spirit depressed', 'cannot hunt for self and kin', 'death', 'difficult', 'discomfort', 'grief and hunger','hard work', 'mental and physical exertion', 'overfastin causes death', 'ravished to the point of ceasing to care for their own maintenance', 'risk of seizure', 'ritual suffering') ~ 'dei',
    costs_3.y %in% c('mating', 'restrict timing of marriages', 'sex') ~ 'mat',
    costs_3.y %in% c('financial/resource', 'horse or slave', 'offerings', 'pays fee', 'sacrificial offerings') ~ 'sca', 
    costs_3.y %in% c('vulnerable to attack', 'quarrels and fights', 'cannot offer food to guests', 'fasting too much is selfish', 'die if break fast', 'unforgivable sin to not fast', 'angry indignations', 'cannot hunt for self and kin', 'cannot work', 'expelled for not fasting', 'failure to follow fast', 'loss of business', 'loss of work', 'loss of work productiity', 'quarrels more common' ) ~ 'se', 
    costs_3.y %in% c('isolation', 'lost in the bush', 'seclusion', 'sleep outside') ~ 'sw', 
    costs_3.y %in% c('NA', 'self-harm', 'special food taboo', 'scratch body with rock', 'loss of impulse control', 'bleeding', 'hemorrhaging from nose and mouth', 'wash in urine?', 'torment', 'scarpe body with stone', 'emesis', 'pilgrimage', 'nose hemorrhage', 'breaks lobes of ears', 'danger', 'deprived of food and water, the life-giving forces of physical life', 'deprivation', 'feasting bad for digestion', 'lose conciousness') ~ '0',
    TRUE ~ costs_3.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$costs_3.y)
table(d$cos3.y)

table(d$costs_4.y)

# Coder 1 Rename for Signals: Create the new column sig1.x based on the conditions


d <- d %>%
  mutate(sig1.x = case_when(
    signal_type.x %in% c('adult group member', 'cultural identity', 'ingroup', 'loyalty', 'loyalty, filiel piety', 'social cohesion', 'special class') ~ 'gm',
    signal_type.x %in% c('faithfulness', 'faith', 'piety', 'devout', 'good Muslim', 'religiousity', 'scrupulous') ~ 'rel',
    signal_type.x %in% c('NA', 'self-control', 'get spirit to take pity', 'good and brave', 'good man', 'protest', 'purity of intention', 'power', 'owed debt', 'power of endurance and self-denial', 'powers of self-denial', 'penance', 'fortitude and powers of endurance', 'dutiful', 'ability to withstand physical ordeals', 'grief', 'brave, virtuous', 'courage', 'degree of love', 'devotion to husband') ~ '0',
    TRUE ~ signal_type.x # Keep the original value if it doesn't match any of the conditions
  ))

table(d$signal_type.x)
table(d$sig1.x)

# Coder 2 Rename for Signals: Create the new column sig1.y based on the conditions

d <- d %>%
  mutate(sig1.y = case_when(
    signal_type.y %in% c('adult group member', 'affiliation', 'acculturation', 'cultural identity', 'group commitment', 'in-group commitment', 'commitment', 'Kanuri identity', 'ingroup', 'loyalty', 'loyalty, filiel piety', 'social cohesion', 'special class', 'tradition/old ways', 'well-to-do') ~ 'gm',
    signal_type.y %in% c('faithfulness', 'faith', 'belief', 'piety', 'pious', 'devout', 'religious commitment', 'good Muslim', 'holiness', 'religiousity', 'scrupulous', 'morality') ~ 'rel',
    signal_type.y %in% c('NA', '1', 'the faster is being punished', 'marriage', 'angry at husband', 'acceptance of marriage', 'corporate head', 'devotion to baby', 'devotion to parents', 'devotion to wife', 'gender norms', 'healing powers', 'health and not pregnant', 'hunt took longer', 'hunter', 'leader', 'leadership', 'magician', 'menstruation', 'mourning', 'mourning and related', 'mourning and status', 'mourning and widow', 'mourning within matriline', 'painted body', 'paternal investment', 'paternal investment?', 'poor quality of food', 'postpartum', 'pre-puberty; purity', 'preparation for guests', 'priesthood', 'privilege', 'puberity', 'puberty', 'resolve conflict', 'rite of passage', 'ritual', 'shaman', 'status', 'strength', 'that something was going to happen', 'they would smear the child face with ashes as a signal to others not to offer food‚Äîa necessary warning when children were customarily fed by everyone', 'warrior', 'weakness', 'work', 'rice availability', 'respect of deceased shaman', 'puberty rite', 'self-control', 'anger from wife', 'apology', 'bodhisattva', 'bride', 'bravery and leadership', 'black smeared on face to signal do not feed', 'bargaining', 'bear hunter', 'adolescence', 'adulthood', 'get spirit to take pity', 'good and brave', 'good man', 'protest', 'purity of intention', 'power', 'owed debt', 'power of endurance and self-denial', 'powers of self-denial', 'penance', 'fortitude and powers of endurance', 'dutiful', 'ability to withstand physical ordeals', 'grief', 'brave, virtuous', 'courage', 'degree of love', 'devotion to husband') ~ '0',
    TRUE ~ signal_type.y # Keep the original value if it doesn't match any of the conditions
  ))

table(d$signal_type.y)
table(d$sig1.y)

# Combine columns
## Create costc.x
# Create costc.x
d$costc.x <- apply(d[, c("cos1.x", "cos2.x", "cos3.x")], 1, function(x) {
  # Remove '0' and NA values from the list
  unique_vals <- unique(na.omit(x[x != "0"]))
  
  # If all values are '0', return '0'
  if (length(unique_vals) == 0 && all(x == "0")) {
    return("0")
  }
  
# Otherwise, return the unique values concatenated by commas
  if (length(unique_vals) > 0) {
    return(paste(unique_vals, collapse = ","))
  } else {
    return(NA)
  }
})

# Create costc.y
d$costc.y <- apply(d[, c("cos1.y", "cos2.y", "cos3.y")], 1, function(x) {
  # Remove '0' and NA values from the list
  unique_vals <- unique(na.omit(x[x != "0"]))
  
  # If all values are '0', return '0'
  if (length(unique_vals) == 0 && all(x == "0")) {
    return("0")
  }
  
  # Otherwise, return the unique values concatenated by commas
  if (length(unique_vals) > 0) {
    return(paste(unique_vals, collapse = ","))
  } else {
    return(NA)
  }
})

table(d$costc.x)

# Create cogc.x
d$cogc.x <- apply(d[, c("cog1.x", "cog2.x", "cog3.x")], 1, function(x) {
  # Remove '0' and NA values from the list
  unique_vals <- unique(na.omit(x[x != "0"]))
  
  # If all values are '0', return '0'
  if (length(unique_vals) == 0 && all(x == "0")) {
    return("0")
  }
  
  # Otherwise, return the unique values concatenated by commas
  if (length(unique_vals) > 0) {
    return(paste(unique_vals, collapse = ","))
  } else {
    return(NA)
  }
})

table(d$cogc.x)

# Create cogc.y
d$cogc.y <- apply(d[, c("cog1.y", "cog2.y", "cog3.y")], 1, function(x) {
  # Remove '0' and NA values from the list
  unique_vals <- unique(na.omit(x[x != "0"]))
  
  # If all values are '0', return '0'
  if (length(unique_vals) == 0 && all(x == "0")) {
    return("0")
  }
  
  # Otherwise, return the unique values concatenated by commas
  if (length(unique_vals) > 0) {
    return(paste(unique_vals, collapse = ","))
  } else {
    return(NA)
  }
})

table(d$cogc.y)


# Coder 1 Unlist cost types
cs <- str_split(d$costc.x, ",")
cs2 <- str_trim(unlist(cs))
cs3 <- unique(cs2)

# Create matrix for cost
mat_cost <- matrix(0, nrow = nrow(d), ncol = length(cs3), dimnames = list(NULL, cs3))

# Fill the matrix
for (i in 1:nrow(d)) {
  cost_types <- str_trim(cs[[i]])  # Trim whitespace for each cost type vector
  for (j in cost_types) {
    mat_cost[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 1 Dataframe costs 
mat_cost_df <- as.data.frame(mat_cost)

# Coder 2 Unlist cost types
csy <- str_split(d$costc.y, ",")
csy2 <- str_trim(unlist(csy))
csy3 <- unique(csy2)

# Create matrix for cost
mat_costy <- matrix(0, nrow = nrow(d), ncol = length(csy3), dimnames = list(NULL, csy3))

# Fill the matrix
for (i in 1:nrow(d)) {
  cost_typesy <- str_trim(csy[[i]])  # Trim whitespace for each cost type vector
  for (j in cost_typesy) {
    mat_costy[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 1 Dataframe costs 
mat_costy_df <- as.data.frame(mat_costy)



# Unlist cog types for Coder 1
cg <- str_split(d$cogc.x, ",")
cg2 <- str_trim(unlist(cg))
cg3 <- unique(cg2)

# Create matrix for cog
mat_cog <- matrix(0, nrow = nrow(d), ncol = length(cg3), dimnames = list(NULL, cg3))

# Fill the matrix
for (i in 1:nrow(d)) {
  cog_types <- str_trim(cg[[i]])  # Trim whitespace for each cost type vector
  for (j in cog_types) {
    mat_cog[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 1 Dataframe cognitive
mat_cog_df <- as.data.frame(mat_cog)


# Unlist cog types for Coder 2
cgy <- str_split(d$cogc.y, ",")
cgy2 <- str_trim(unlist(cgy))
cgy3 <- unique(cgy2)

# Create matrix for cog 2
mat_cogy <- matrix(0, nrow = nrow(d), ncol = length(cgy3), dimnames = list(NULL, cgy3))

# Fill the matrix
for (i in 1:nrow(d)) {
  cogy_types <- str_trim(cgy[[i]])  # Trim whitespace for each cost type vector
  for (j in cogy_types) {
    mat_cogy[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 2 Dataframe cognitive
mat_cogy_df <- as.data.frame(mat_cogy)


# Unlist sig types for Coder 1
sg <- str_split(d$sig1.x, ",")
sg2 <- str_trim(unlist(sg))
sg3 <- unique(sg2)

# Create matrix for sig
mat_sg <- matrix(0, nrow = nrow(d), ncol = length(sg3), dimnames = list(NULL, sg3))

# Fill the matrix
for (i in 1:nrow(d)) {
  sg_types <- str_trim(sg[[i]])  # Trim whitespace for each cost type vector
  for (j in sg_types) {
    mat_sg[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 1 Dataframe sig
mat_sg_df <- as.data.frame(mat_sg)

# Unlist sig types for Coder 1
sgy <- str_split(d$sig1.y, ",")
sgy2 <- str_trim(unlist(sgy))
sgy3 <- unique(sgy2)

# Create matrix for sigy
mat_sgy <- matrix(0, nrow = nrow(d), ncol = length(sgy3), dimnames = list(NULL, sgy3))

# Fill the matrix
for (i in 1:nrow(d)) {
  sgy_types <- str_trim(sgy[[i]])  # Trim whitespace for each cost type vector
  for (j in sgy_types) {
    mat_sgy[i, j] <- 1  # Set matrix entry to 1 where cost type is present
  }
}

# Coder 2 Dataframe sig
mat_sgy_df <- as.data.frame(mat_sgy)

# List of dataframes and vars for full run dataframe

# mat_cog_df
mat_cog_df <- mat_cog_df %>%
  rename(
    asc.x = asc,
    sb.x = sb,
    vk.x = vk
  )

# mat_cogy_df
mat_cogy_df <- mat_cogy_df %>%
  rename(
    asc.y = asc,
    vk.y = vk
  )

# mat_cost_df
mat_cost_df <- mat_cost_df %>%
  rename(
    sca.x = sca,
    mat.x = mat,
    dei.x = dei,
    sw.x = sw,
    se.x = se
  )

# mat_costy_df
mat_costy_df <- mat_costy_df %>%
  rename(
    sca.y = sca,
    mat.y = mat,
    dei.y = dei,
    se.y = se
  )

# mat_sg_df
mat_sg_df <- mat_sg_df %>%
  rename(
    gm.x = gm,
    rel.x = rel
  )
# mat_sgy_df
mat_sgy_df <- mat_sgy_df %>%
  rename(
    gm.y = gm,
    rel.y = rel
  )

# Gender.x
# Gender.y

# `time lapse.x`
# `time lapse.y`

# leader.x
# leader.y

# Select specific columns from each dataframe
df_combined <- cbind(
  mat_cost_df[, c("sca.x", "mat.x", "dei.x", "sw.x", "se.x")],
  mat_costy_df[, c("sca.y", "mat.y", "dei.y", "se.y")],
  mat_cog_df[, c("asc.x", "vk.x")],
  mat_cogy_df[, c("asc.y", "vk.y")],
  mat_sg_df[, c("gm.x", "rel.x")],
  mat_sgy_df[, c("gm.y", "rel.y")],
  d[, c("Gender.x", "Gender.y", "time_lapse.x", "time_lapse.y", "leader.x", "leader.y", "ID", "Excerpt.x")]
)

# Add a new column 'sw.y' initialized with 0 for each row
df_combined$sw.y <- 0

# Load the writexl package
library(writexl)

# Export df_combined to the specified path as an Excel file
write_xlsx(df_combined, "/Users/kristensyme/Desktop/Leveraging the Power of LLMs/df_combined.xlsx")

# Create final data set for analysis
asc <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsasc.xlsx")
dei <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsdei.xlsx")
gen <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsgen2.xlsx")
gm <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsgm.xlsx")
lap <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphslap.xlsx")
lead <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphslead.xlsx")
mat <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsmat.xlsx")
rel <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsrel.xlsx")
sca <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphssca.xlsx")
se <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsse.xlsx")
sw <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsse.xlsx")
vk <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsvk.xlsx")

# List of data frame names
data_frames <- list(asc, dei, gen, gm, lap, lead, mat, rel, sca, se, sw, vk)

#Combine the data frames by binding columns
data <- do.call(cbind, data_frames)

# Remove duplicated columns by selecting unique names
data <- data[ , !duplicated(names(data))]

install.packages("writexl")
library(writexl)
write_xlsx(data, "/Users/kristensyme/Desktop/Leveraging the Power of LLMs/final data llms.xlsx")
write_xlsx(data, "/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/final data llms.xlsx")










