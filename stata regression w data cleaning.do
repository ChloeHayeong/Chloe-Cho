**checking the coding of the focal variables****
codebook X2MSCALK5 X2RSCALK5 X2TCHEXT 
sum X2MSCALK5 
sum X2RSCALK5
sum X2TCHEXT
sum X2TCHINT 
tab p1read 
tab p1spank

**recoding *** 
recode X2MSCALK5 (missing=.), gen(math)
recode X2RSCALK5 (missing=.), gen(reading)
recode X2TCHEXT (missing=.), gen(extbeh)
recode X2TCHINT (missing=.), gen(intbeh)
recode p1read (missing=.), gen(pread)
recode p1spank (missing=.), gen(pspank)

***generating the compostie variables of behavior problems ****
gen behavior = (extbeh + intbeh)/2

****generating the overall performance variables with math and literacy scores ****
gen performance = (math + reading)/2

***checkig correlation with the covariates****
pwcorr math reading extbeh intbeh pread pspank X12PAR1ED X2DISABL P2HEALTH MOM_DEPRESS, sig star(.05)

pwcorr math reading extbeh intbeh pread pspank P2SAFEPL P2DRUG P2BURGLR, sig star(.05)


** recode covariate variables ***
tab P2HEALTH
recode P2HEALTH (-9=.) (-8=.) (-7=.) (-1=.), gen(phelath)
tab phelath

tab P2SAFEPL
recode P2SAFEPL (-9=.) (-8=.) (-7=.), gen(psafe)

tab P2DRUG
recode P2DRUG (-9=.) (-8=.) (-7=.), gen(pdrug)

tab P2BURGLR
recode P2BURGLR (-9=.) (-8=.) (-7=.), gen(burglr)

tab X2DISABL
recode X2DISABL (1=0) (2=1), gen(disabl)

****#3 descirptive statistics for the final set of variables****
summarize math reading extbeh intbeh pread pspank performance behavior

summarize X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences

***checking correlation with the focal variables ****
pwcorr math reading extbeh intbeh pread pspank performance behavior, sig star(.05)

**imputation started 
mi set flong
browse _mi_m _mi_id

misstable sum, gen(m_)

mcartest pread pspank performance behavior 

mi misstable patterns math reading extbeh intbeh pread pspank performance behavior X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences


mi register imputed math reading extbeh intbeh pread pspank performance behavior X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences

mi describe 

tab pread

mi impute chained (regress) math reading pspank performance school_absences pread extbeh intbeh P2INVL (logit) MOM_DEPRESS disabl(mlogit) phelath psafe pdrug burglr(ologit) X12PAR1ED_I, augment  add (20) rseed (123) dots

edit _mi_m
sort _mi_id
edit _mi_m _mi_id
sort _mi_id _mi_m

mi xeq: sum math reading extbeh intbeh pread pspank performance behavior
mi xeq: corr math reading extbeh intbeh pread pspank performance behavior 


mi estimate, esampvaryok vartable cmdok dots: regress reading pread X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences 
mi estimate, esampvaryok vartable cmdok dots: regress math pread X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences 
mi estimate, esampvaryok vartable cmdok dots: regress performance pread X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences
mi estimate, esampvaryok vartable cmdok dots: regress extbeh pspank X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences
mi estimate, esampvaryok vartable cmdok dots: regress intbeh pspank X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences
mi estimate, esampvaryok vartable cmdok dots: regress beh pspank X12PAR1ED_I disabl phelath P2INVL MOM_DEPRESS psafe pdrug burglr school_absences

