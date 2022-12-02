## January 23rd

- Started to create files in github to try and work out of the repo more
- Nature paper that says Coronavirus is linked to white nose so I can explain to my friends what I do
https://www.nature.com/articles/s41598-018-33975-x
UPDATE: Different coronavirus but still pretty cool paper
- Decided that for first few simulations only 10 will suffice
- Need to still confirm with Adrian size of validation set after it has been full scraped

## January 27th

- Forgot to update journal over weekend and push stuff to Git...oops
- Did several odd things to SIR model to have SSIR and SSIIRR or some combination of those
- Ultimately went with Mike's selection of SSII model and include R in the later part once we have a grasp on things
- Use of Morgan's bird and mosquito model
- Look into making a Make rule for this set of equations and functions of everything to match Adrians
- Wait for Adrians things to not be broken from Make (they're probably not but I am not good at Make)
- Get a second opinion on my scripts
- Unsure if my scripts are going to be in the same format as the geocache datascrape
- Have to look into push and pull from R so I dont just do it by hand
- I think Gamma Distributed County Duration isn't a thing
- Do I need more scripts? I think the remainder ones are on Git via Adrian
- TO DO: Upload WNS-GT-Duration.R and fix what else is up with it. Ultimately it is the one needed for frag-runner.R which
is the script used for creating simulations 


## Feb 2nd

- create a probability for the edges of user shared visits for all possible edges (ie visited a and b divided by total visits between the two)
- Ask Ben about the probability between nodes and how that should be changed to adjust for recurring visitors
- Should the network allow for returning visitors or not since it would effect the probabilities 
- Or should simply the probability not include those returning ones (Is that allowed ??)


## Feb 8th

- Get better at using the journal to test what is needed. Start to incorporate our data with the model. 
- Examined the format Adrian has organized the data and how to incorporate it into the model
- Attempt the proof Ben asked for of binomial cloglog
- Get rid of SSIIRR
- Go back to SIR with a simpler binomial transition
- Dont worry about recurring visitors
- Changed countydistribution.R accordingly
- Get a better understanding of what order to run everything in (drake files) 
- Learn about ABC vs MCMC
- Is ABC with sequential Monte carlo the answer?
- A much nicer organization method is needed for all this. Huge mess
- Currently GLM is built on occurance (ie. poisson dist based on date). This seems fine but unsure what to change to binom.

## Feb 10th

- Spoke with Adrian about data output and how the data will be in format long
- Found various methods of people using complementary log log in binomial regression
- Cant actually prove it because I cant wrap my head around the proof start
- updated GLMmodelfitcavedata.R and CountyDurationScript.R 
- Understood data format and how our most basic model will be interpretted
