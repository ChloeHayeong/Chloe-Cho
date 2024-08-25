* Encoding: UTF-8.
jttk* Encoding: UTF-8.

* Parallel Analysis Program For Raw Data and Data Permutations.

* This program conducts parallel analyses on data files in which
  the rows of the data matrix are cases/individuals and the
  columns are variables;  Data are read/entered into the program
  using the GET command (see the GET command below);  The GET 
  command reads an SPSS systemfile, which can be either the 
  current, active SPSS data file or a previously saved systemfile;
  A valid filename/location must be specified on the GET command;
  A subset of variables for the analyses can be specified by using
  the "/ VAR =" subcommand with the GET statement;  There can be
  no missing values.

* You must also specify:
  -- the # of parallel data sets for the analyses;
  -- the desired percentile of the distribution and random
     data eigenvalues;
  -- whether principal components analyses or principal axis/common
     factor analysis are to be conducted, and
  -- whether normally distributed random data generation or 
     permutations of the raw data set are to be used in the
     parallel analyses.

* WARNING: Permutations of the raw data set are time consuming;
  Each parallel data set is based on column-wise random shufflings
  of the values in the raw data matrix using Castellan's (1992, 
  BRMIC, 24, 72-77) algorithm; The distributions of the original 
  raw variables are exactly preserved in the shuffled versions used
  in the parallel analyses; Permutations of the raw data set are
  thus highly accurate and most relevant, especially in cases where
  the raw data are not normally distributed or when they do not meet
  the assumption of multivariate normality (see Longman & Holden,
  1992, BRMIC, 24, 493, for a Fortran version); If you would
  like to go this route, it is perhaps best to (1) first run a 
  normally distributed random data generation parallel analysis to
  familiarize yourself with the program and to get a ballpark
  reference point for the number of factors/components;
  (2) then run a permutations of the raw data parallel analysis
  using a small number of datasets (e.g., 10), just to see how long
  the program takes to run; then (3) run a permutations of the raw
  data parallel analysis using the number of parallel data sets that
  you would like use for your final analyses; 100 datasets are 
  usually sufficient, although more datasets should be used if
  there are close calls.


************************ PARALLEL ANALYSIS COMMANDS

set mxloops=9000 printback=off width=80  seed = 1953125.
matrix.

* Enter the name/location of the data file for analyses after "FILE =";
  If you specify "FILE = *", then the program will read the current,
  active SPSS data file;  You can alternatively enter the name/location
  of a previously saved SPSS systemfile instead of "*";
  you can use the "/ VAR =" subcommand after "/ missing=omit"
  subcommand to select variables for the analyses.

GET raw / FILE = *
 / missing=omit / VAR = rap1 rap2 rap3 rap4 rap5 rap6 rap7 rap8 rap9 rap10 rap11 rap12 rap13 rap14 rap15 rap16 rap17 rap18 rap19.

* Enter the desired number of parallel data sets here.
compute ndatsets = 1000.

* Enter the desired percentile here.
compute percent  = 95.

* Enter either
  1 for principal components analysis, or
  2 for principal axis/common factor analysis.
compute kind = 1 .

* Enter either
  1 for normally distributed random data generation parallel analysis, or
  2 for permutations of the raw data set (VERY time consuming).
compute randtype = 1.

* End of required user specifications.


compute ncases   = nrow(raw). 
compute nvars    = ncol(raw).

* principal components analysis & random normal data generation.
do if (kind = 1 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* principal components analysis & raw data permutation.
do if (kind = 1 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute realeval = eval(d * vcv * d).
compute evals = make(nvars,ndatsets,-9999).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute evals(:,#nds) = eval(d * vcv * d).
end loop.
end if.

* PAF/common factor analysis & random normal data generation.
do if (kind = 2 and randtype = 1).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = sqrt(2 * (ln(uniform(ncases,nvars)) * -1) ) &*
            cos(6.283185 * uniform(ncases,nvars) ).
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* PAF/common factor analysis & raw data permutation.
do if (kind = 2 and randtype = 2).
compute nm1 = 1 / (ncases-1).
compute vcv = nm1 * (sscp(raw) - ((t(csum(raw))*csum(raw))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute cr = (d * vcv * d).
compute smc = 1 - (1 &/ diag(inv(cr)) ).
call setdiag(cr,smc).
compute realeval = eval(cr).
compute evals = make(nvars,ndatsets,-9999).
compute nm1 = 1 / (ncases-1).
loop #nds = 1 to ndatsets.
compute x = raw.
loop #c = 1 to nvars.
loop #r = 1 to (ncases -1).
compute k = trunc( (ncases - #r + 1) * uniform(1,1) + 1 )  + #r - 1.
compute d = x(#r,#c).
compute x(#r,#c) = x(k,#c).
compute x(k,#c) = d.
end loop.
end loop.
compute vcv = nm1 * (sscp(x) - ((t(csum(x))*csum(x))/ncases)).
compute d = inv(mdiag(sqrt(diag(vcv)))).
compute r = d * vcv * d.
compute smc = 1 - (1 &/ diag(inv(r)) ).
call setdiag(r,smc).
compute evals(:,#nds) = eval(r).
end loop.
end if.

* identifying the eigenvalues corresponding to the desired percentile.
compute num = rnd((percent*ndatsets)/100).
compute results = { t(1:nvars), realeval, t(1:nvars), t(1:nvars) }.
loop #root = 1 to nvars.
compute ranks = rnkorder(evals(#root,:)).
loop #col = 1 to ndatsets.
do if (ranks(1,#col) = num).
compute results(#root,4) = evals(#root,#col).
break.
end if.
end loop.
end loop.
compute results(:,3) = rsum(evals) / ndatsets.

print /title="PARALLEL ANALYSIS:".
do if (kind = 1 and randtype = 1).
print /title="Principal Components & Random Normal Data Generation".
else if (kind = 1 and randtype = 2).
print /title="Principal Components & Raw Data Permutation".
else if (kind = 2 and randtype = 1).
print /title="PAF/Common Factor Analysis & Random Normal Data Generation".
else if (kind = 2 and randtype = 2).
print /title="PAF/Common Factor Analysis & Raw Data Permutation".
end if.
compute specifs = {ncases; nvars; ndatsets; percent}.
print specifs /title="Specifications for this Run:"
 /rlabels="Ncases" "Nvars" "Ndatsets" "Percent".
print results 
 /title="Raw Data Eigenvalues, & Mean & Percentile Random Data Eigenvalues"
 /clabels="Root" "Raw Data" "Means" "Prcntyle"  /format "f12.6".

compute root      = results(:,1).
compute rawdata = results(:,2).
compute percntyl = results(:,4).

save results /outfile=* / var=root rawdata means percntyl .

end matrix.

* plots the eigenvalues, by root, for the real/raw data and for the random data;
  This command works in SPSS 12, but not in all earlier versions.

TSPLOT VARIABLES= rawdata means percntyl /ID= root /NOLOG.
