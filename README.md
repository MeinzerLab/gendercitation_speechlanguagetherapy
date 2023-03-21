# Gender citation analysis in speech-language therapy
This repository contains the gendercitation code by Jordan Dworkin (https://github.com/jdwor/gendercitation) adapted for our gender citation analysis in speech and language therapy. We only upload R files that were adapted by us. The rest of the code can be found at the linked repository.
Code changes are marked as seen in the example below from step 6:

```r
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Changed the api to get the gender probabilities.
## original code:
## json_file=paste0("https://gender-api.com/get?name=",this_name,
##                  "&key=",gender_api_key)

# Pull json data from gender-api
json_file=paste0("https://api.genderize.io/?name=",this_name)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++
```
