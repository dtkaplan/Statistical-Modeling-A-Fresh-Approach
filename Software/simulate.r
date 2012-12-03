#########################################
# Simulation of Data
#########################################
# Must be recurrent: each var can depend 
# only on the previously listed vars.
# 
# Use 'nsamps' to specify the size of inputs

# B causes A
B.to.A = list(
  vars = list(B=NULL, A=NULL),
  eqn = c(
    'rnorm(nsamps)',
    'B + rnorm(nsamps)')
);
class(B.to.A) = 'cnet';

# C causes both A and B
cnet.common.cause = list(
  vars = list(C=NULL, B=NULL,A=NULL),
  eqn = c(
    'rnorm(nsamps)',
    'rnorm(nsamps)+C',
    'rnorm(nsamps)-C')
);
class(cnet.common.cause) = 'cnet';

# C causes both A and B
cnet.witness = list(
  vars = list(A=NULL, B=NULL, C=NULL),
  eqn = c(
    'rnorm(nsamps)',
    'rnorm(nsamps)',
    'rnorm(nsamps) - A + B')
);
class(cnet.witness) = 'cnet';

# Mediator --- A <- C <- B 
cnet.mediator= list(
  vars = list(B=NULL, C=NULL, A=NULL),
  eqn = c(
    'rnorm(nsamps)',
    'B + rnorm(nsamps)',
    'rnorm(nsamps) - C')
);
class(cnet.mediator) = 'cnet';

`A<-C->B` <- cnet.common.cause
`A->C<-B` <- cnet.witness
`A<-C<-B` <- cnet.mediator 
cnet.chain <- cnet.mediator # For backward compatibility.  Deprecate this.

jock = list(
  vars = list( IQ=NULL, Athletic=NULL, College=NULL, Varsity=NULL),
  eqn = c(
     'rnorm(nsamps,mean=100,sd=15)',
     'rnorm(nsamps,mean=20,sd=3)',
     'boolean.to.factor((IQ + 3*Athletic + rnorm(nsamps,mean=0,sd=5))>170)',
     "boolean.to.factor(College=='Yes' & (Athletic+rnorm(nsamps,sd=2)  > 25))")
);
class(jock) = 'cnet';

campaign.spending = list(
    vars = list( popularity=NULL, polls=NULL, spending=NULL, vote=NULL ),
    eqn = c(
        "runif(nsamps, min=15,max=85)",
        "popularity + rnorm(nsamps,sd=3)",
        "100 - polls + rnorm(nsamps,sd=10)",
        "0.75*popularity + 0.25*spending + rnorm(nsamps,sd=5)" )
);
class(campaign.spending) = 'cnet';

university.test = list(
 vars = list(stdev=10, student=NULL, when=NULL, university=NULL, score=NULL),
 hidden=c(1),
 eqn = c(
  "1",
  "factor(paste('sID',rep( round(runif(nsamps*5,min=10000000,max=20000000)), each=2),sep=''))",
   "as.factor(c('entrance','graduation')[1+ (1:length(student))%%2])",
   "as.factor(c('UnivA','UnivB','UnivC','UnivD','UnivE')[1+as.numeric(student)%%5])",
   "round(c(0,20,40,60,100)[as.numeric(university)]*as.numeric(when=='graduation') + c(500,490,480,480,470)[as.numeric(university)] + rnorm(length(student),mean=0,sd=stdev))"
    )
);
class(university.test) = 'cnet';


electro = list(
    hidden=c(3), # don't return the underlying rate
    vars = list( animal=NULL, genotype=NULL, rate=NULL, interval=NULL),
    eqn = c(
      "rep(1:10,each=nsamps)",
      "c('Wild-type','Mutant')[1+animal%%2]",
      "rep(runif(1, .25, .75),each=nsamps) + .1*(animal%%2)",
      "rexp( 10*nsamps, rate=rate )")
);
class(electro) = 'cnet';
      
heights = list(
    vars = list( mgenes=NULL, fgenes=NULL, kgenes=NULL,
                Sex=NULL,
                MHeight=NULL,FHeight=NULL,KHeight=NULL),
    eqn = c(
       "rnorm(nsamps, mean=0,sd=2)",
       "rnorm(nsamps, mean=0,sd=2)",
       ".5*mgenes + .5*fgenes",
       "sample(c('F','M'),nsamps,replace=TRUE)",
       "mgenes + rnorm(nsamps,mean=69,sd=2.5)",
       "fgenes + rnorm(nsamps,mean=65,sd=2.5)",
       "kgenes + rnorm(nsamps,mean=69,sd=2.5) - 4*(Sex=='F')"  )
);
class(heights) = 'cnet';

cnet.unconnected.causes = list(
   vars = list( C = NULL, B = NULL, A = NULL),
   eqn = c(
        "rnorm( nsamps )",
        "rnorm( nsamps )",
        "B + C + rnorm(nsamps,sd=.1)")
);
class(cnet.unconnected.causes) = 'cnet';


cnet.connected.causes = list(
   vars = list( C = NULL, B = NULL, A = NULL),
   eqn = c(
        "rnorm( nsamps )",
        "C + rnorm( nsamps,sd=.1 )",
        "B + C + rnorm(nsamps,sd=.1)")
);
class(cnet.connected.causes) = 'cnet';

cnet.correlated.causes = list(
   hidden=c(1),  # don't display the first variable
   vars = list( D = NULL, C = NULL, B = NULL, A = NULL),
   eqn = c(
        "rnorm( nsamps )",
        "D+rnorm(nsamps,sd=.1)",
        "D+rnorm(nsamps,sd=.1)",
        "B + C + rnorm(nsamps,sd=.1)")
);
class(cnet.correlated.causes) = 'cnet';

aspirin = list(
   vars = list( sick=NULL, mgPerDay=NULL, stroke=NULL ),
   eqn = c(
       "sample(c(rep(0,20),1:10,1:3,8:10), nsamps,replace=TRUE)",
       "5*(sick<2)*sample(c(0,0,0,0:10,0:20),nsamps, replace=TRUE) + (sick>=2)*5*sample(c(0,0,0,0:20,10:20,15:20),nsamps,replace=TRUE)",
       "cut(rnorm(nsamps,mean=0,sd=5) + 2*sick - mgPerDay/10, c(-Inf,5,Inf), labels=c('N','Y'))")
);
class(aspirin) = 'cnet';
# ============================


salaries = list(
   vars = list( age=NULL, sex=NULL, children=NULL,rank=NULL, salary=NULL ),
   eqn = c(
       "round(runif(nsamps, min=32, max=65) + rnorm(nsamps,sd=4))",
       "cut( (age-50)*(age>50) + rnorm(nsamps,sd=10), c(-Inf,0,Inf),labels=c('F','M') )",
       "sample(c(0,0,0,1,1,1,2,2,3),nsamps,replace=TRUE)",
       "cut( age + 2*(sex=='M') -sample(c(0,1),nsamps,replace=TRUE)*(sex=='F')*children^2  + rnorm(nsamps,sd=3),c(-Inf,38,47,Inf),labels=c('Assist','Assoc','Full'))",
       "20000+500*age + rnorm(nsamps,sd=2000) + 5000*(rank=='Assoc') + 10000*(rank=='Full')")
);
class(salaries) = 'cnet';
# ============================
bogus.groups = list(
  vars = list( ngroups=5, group=NULL, val=NULL),
  hidden=c(1),
  eqn = c(
    "5",
    "rep(LETTERS[1:ngroups[1] ], each=nsamps)",
    "rnorm(ngroups[1]*nsamps, mean=100, sd=15)"
    )
  );
class(bogus.groups) = 'cnet';
# a specialized program for analysis of these bogus-group data
# control sets whether to compare the groups to a control group.
# otherwise
compare.many.groups = function(d, control=NULL){
  compare.two = function(one,two,ind) {
      sset = subset( d, group==one | group==two )
      sset$group = as.factor(as.character(sset$group))
      foo = summary(lm( val ~ group, data=data.frame(sset) ) )
      group1[ind] <<- one
      group2[ind] <<- two
      pvalue[ind] <<- foo$coef[2,4]
      differ[ind] <<- foo$coef[2,1]
      return(TRUE)
    }
  gps = levels(d$group)
  ncomps = ifelse(is.null(control),
    length(gps)*(length(gps)-1)/2,
    length(gps)-1)
  group1 = rep("",ncomps)
  group2 = rep("",ncomps)
  pvalue = rep(0,ncomps)
  differ = rep(0,ncomps)
  ind = 0
  if (is.null(control)) {
    for (j in 1:(length(gps)-1) ) {
      for (k in (j+1):length(gps) ) {
        ind = ind+1
        one = gps[j]
        two = gps[k]
        compare.two( one, two, ind )
      }
    }
  }
  else {
    if( !any(control==gps) )
      stop("Control group not in the data.")
    else {
      for (k in 1:length(gps)) {
        if (gps[k] != control) {
          ind = ind+1
          two = gps[k]
          compare.two( control, two, ind )
        }
      }
    }
  }
  return( data.frame( group1=group1, group2=group2, diff=differ, pvalue=pvalue))
}

##########################################
# Utility functions
names.cnet = function(net){names(net$vars)}

equations = function(net) {
  if (class(net) == 'cnet.hidden') return( 'hidden');
  nm = names( net$vars )
  for (k in 1:length(nm)){
      cat((paste(nm[k], '<==', net$eqn[k],'\n')))
  }
  invisible(net)
}
  

print.cnet = function(net) {
  foo = influences(net);
  N = names(foo);
  cat(paste('Causal Network with ',length(N),' vars: ',
            paste(N,collapse=', '),
            '\n===============================================\n'))
  for (k in 1:length(N)){
    if (length(foo[[k]])==0) {
      cat((paste(N[k], 'is exogenous\n', collapse=' ')) )}
    else{
      cat((paste(N[k],'<==', paste(foo[[k]],collapse=' & '),'\n')))}
  }
  invisible(net)
}

influences = function(net) {
  foo = net$vars;
  N = names( net$vars )
  for (k in 1:length(net$eqn) ) {
      goo = net$eqn[[k]];
      goo = all.vars(parse(text=goo))
      keep = goo %in% N;
      foo[[k]] = goo[keep]
  }
  return(foo)
}

boolean.to.factor = function(bool, levels=c('No','Yes')){
  res = rep(levels[1], length(bool));
  res[bool] = levels[2];
  return( as.factor(res) )
}
      
############################################
# For hidden cnets --- class cnetHidden
names.cnetHidden = function(net){names(net$vars)}
print.cnetHidden = function(net){
  cat('The internal structure of this cnet is hidden.\n')
  cat('The variables are:\n')
  cat(names(net) )
  cat('\n')
}
  
  
##########################################
# The main function for running things  

run.sim = function(net, n=5, inject=FALSE, seed=NULL,...){
  if( !is.null(seed) ) set.seed(seed)
  nsamps = n;

  # Transfer the experimental variables listed in ... to the 
  # net structure so that those variables are set.
  L = list(...);
  vnames = names(net$vars);
  enames = names(L)
  # Check for errors in the argument list
  if (length(enames) > 0 ){
  for (k in 1:length(enames)){
    if (! enames[k] %in% vnames ) {
        warning(paste('**', enames[k],"** not a variable in the network. Check spelling."))
    }
  } 
  }

  for (k in 1:length(L)) {
    ind = which( enames[k] == vnames);
    if (length(ind) > 0 ) {
       net$vars[[ind]] = L[[k]];
    }
  }

  # Run through the equations to set each variable in turn
  # If the variable is already set (experimentally, with ...)
  # then just leave it at that.
  for( k in 1:length(net$vars) ){
     if (is.null(net$vars[[k]]) | inject ){    
        # figure out what the network says it should be naturally
        hoo = eval( parse(text=net$eqn[[k]]), envir=net$vars)
        if (inject & !is.null(net$vars[[k]])) {
            # add the experimental value to the natural value
            # that is, an incremental experiment.
            hoo = hoo + rep(net$vars[[k]], length.out=nsamps);
        }
     } 
     else {
        # copy it from the value set in the argument list
          hoo = rep(net$vars[[k]], length.out=nsamps);
     }

    net$vars[[k]] = hoo;
  }

  ret = data.frame(net$vars)
  
  post = net[['post']]
  if (!is.null(post) ){
    ret = post(ret)
  }

  hidden = net[['hidden']]
  if( !is.null(hidden) ){
    ret = ret[,-hidden]
  }
  
  return(ret)
}

######################################
# See problem f2008-6 and f2008-5 (of which this is a simulation)

economic.outlook.poll = list(
   vars = list( rawage=NULL, rawincome=NULL, rawpessimism=NULL, age=NULL, income=NULL, pessimism=NULL ),
   hidden = 1:3,
   eqn = c(
       "runif(nsamps,min=18,max=80)",
       "runif(nsamps,min=-4,max=120) - 50*(rawage<30)",
       "120-rawincome + rawage" , 
       "cut(rawage, breaks=c(18,29,39,64,Inf),
         labels=c('[18 to 29]', '[30 to 39]', '[40 to 64]', '[65 and older]'),
         include.lowest=TRUE)",
       "cut(rawincome, breaks=c(-Inf,20,35,50,100,Inf),
         labels=c('[less than $20000]', '[$20,000 to $34,999]',
         '[$35,000 to $49,999]', '[$50,000 to $99,999]', '[$100,000 or more]'), 
                  include.lowest=TRUE)",
       "pmin(10,pmax(0,round(rawpessimism/17)))")
     );
class(economic.outlook.poll) = 'cnet';

#########################################
# See problem f2008/Survival/survival-bias.tex
survival = list(
   hidden = c(4,5),
   vars = list( admitted=NULL,survival=NULL,death=NULL,start='12/1/2000',end='12/1/2000',label='NA'),
   eqn = c(
       "as.Date('9/16/1990', '%m/%d/%Y') + runif(nsamps,0,10000)",
       "round( runif(nsamps,0,5) + rexp(nsamps,rate=1/600) )",
       "admitted + survival"),
# include only the ones that fall between the start and end dates
  post = function(d){
    startnum = as.Date(d$start, '%m/%d/%Y')
    endnum = as.Date(d$end, '%m/%d/%Y')
    d = subset(d, d$admitted <= endnum & d$death >= startnum)
    return(d) }
  );
class(survival) = 'cnet';

#################################
# Vitamin D and systolic blood pressure, with race as a covariate.
vitaminD = list(
 vars = list(magic=20, race=NULL, D=NULL, systolic=NULL ),
 hidden=c(1),
 eqn = c(
  "1",
  "resample(c('B','W','W','W'), nsamps)",
  "pmax(0,round(rnorm(nsamps,mean=37.7,sd=10)*(race=='B') + rnorm(nsamps,mean=71.8,sd=20)*(race=='W')))",
  "pmax(80,round(130 + rnorm(nsamps, mean=0, sd=8) + rexp(nsamps,rate=.15) + magic*(30-D)/200))"
    )
);
class(vitaminD) = 'cnet';

# vitamin D values for Blacks and whites are from Kevin McKinney et al.,
# Association of Race, Body Fat and Season with Vitamin D Status Among
# Young Women: A Cross-sectional Study, Clin. Endocrinol. 2008:69(4):535-541
# http://www.medscape.com/viewarticle/584518

# =====================
# Heart rate during standing and sitting.  Delta sets the difference.
fix.hr.experiment = function(L) {
  standL = L;
  standL$posture=rep("standing", nrow(L))
  standL$hr = L$hr + L$delta + rnorm(nrow(L),sd=3) + (L$age - 55)/50
  hoo = rbind(L,standL)
  hoo = hoo[,-1]
  hoo$age = round(floor(hoo$age))
  hoo$hr = round(hoo$hr)
  inds = order( as.character(hoo$subject) )
  hoo = hoo[inds,]
  return(hoo)
}

hr.experiment = list(
     vars = list(delta=2, age=NULL, subject=NULL, posture=NULL, hr=NULL),
     eqn = c(
       "'set by command: default 2'",
       "runif(nsamps,min=18,max=90)",
       "paste('Subj', 1050 + (1:nsamps), sep='')",
       "rep('sitting',nsamps )",
       "c(70 + rnorm(nsamps,sd=5))"
       ),
      post = fix.hr.experiment
  );
class(hr.experiment) = "cnet";

#################################
# Library book simulatio
#######################
select.books = function(howmany=25,nshelves=4){
  ylim = c(0,nshelves)
  xmax = 20
  bookmax = 80
  books = list()
  heights = list()
  colors = c(terrain.colors(20),topo.colors(20),heat.colors(20))
  draw.a.book = function(k,j,color=NULL,border="gray") {
      h = heights[[k]][j]
      left = books[[k]][j]
      right = books[[k]][j+1]
      if( is.null(color) ) color = colors[ceiling(runif(1,0,length(colors)))]
      polygon( c(left,right,right,left,left), k-1+c(.02,.02, h, h, .02) ,
              col=color,border=border)
  }    
  for (k in 1:nshelves) {
    books[[k]] = c(0,cumsum(pmin(.6,pmax(.05,rexp(bookmax,rate=5)))))
    heights[[k]] = runif(bookmax,.5, .9)
  }
  plot( 0:20, ylim = c(0,nshelves), xlim=c(0,xmax), type='n', bty='n',xaxt="n", yaxt="n",xlab="",ylab="")
  for (k in 1:nshelves){
    lines( c(0,xmax), c(k,k)-1, col="black", lwd=5)
    for (j in 1:bookmax) {
       draw.a.book(k,j)
    }
  }
  bookwidths = c()
  shelves = c()
  for (k in 1:nshelves) {
    bookwidths = c(bookwidths, diff(books[[k]]))
    shelves = c(shelves, rep(k,bookmax))
  }
  bookwidths = 4*ceiling( 457*bookwidths )
  bookset = data.frame( pages=bookwidths, shelf=shelves)
  selected = rep(0, bookmax*nshelves)
  select.a.book = function() {
    L = locator(1)
    shelf = ceiling( L$y )
    booknum = max( which( L$x > books[[shelf]]) )
    if (booknum < 0) booknum = 1
    if (booknum > bookmax) booknum = bookmax
    draw.a.book(shelf,booknum,color='white',border='white')
    return(c(shelf,booknum))
  }
  nselected = 0;
  while (nselected < howmany ){
    foo =select.a.book()
    selected[(foo[1]-1)*bookmax + foo[2]] = 1
    nselected = sum(selected)
  }
bookset$selected = as.factor(c("No","Yes")[1+selected])
  return(bookset)
  
}
##############################
# Mutual funds simulation used in 3.51, f2009-p109
# simulation of mutual funds
mutual.funds = function(n=100,mgrowth=1.06,sdgrowth=0.05,nyears=5,new.money=1000,orig.money=10000) {
  name = function(k,prefix='year'){paste(prefix,k,sep='')}
  funds = list( )
  starting = runif(n, 10,50)
  starting = (orig.money/n)*(starting / mean(starting))
  funds[[name(0)]] = starting
  cum.growth = rep(1,n)
  killed = rep(FALSE,n)
  for (k in 1:nyears ) {
    growth = rnorm(n, mean=mgrowth, sd=sdgrowth)
    # cumulative growth up to the point where a fund was killed
    cum.growth = ifelse(killed,cum.growth,cum.growth*growth)
    funds[[name(k)]] = growth*funds[[name(k-1)]]
    # kill off funds with cumulative losses
    increase = cum.growth
    if (k == 1 ) {# decreased in the first year
      kill = which(  increase < 1.00 )
    }
    else {# growing half as fast as the others
      kill = which( increase < (1 + (mgrowth-1)/2)^k )
    }
    # reinvest the money from the killed funds along with the new money
    add.to.funds = new.money +
      ifelse(length(kill)>0, funds[[name(k)]][kill], 0)
    funds[[name(k)]][kill] = 0
    killed[kill] = TRUE
    # put the new funds in those which have grown more than mgrowth
    # in the last year
    growth.funds = which(funds[[name(k)]] > 0 & growth > mgrowth)
    if (is.null(growth.funds)) {
      # if there aren't any, just split them among the surviving funds
      growth.funds = which( funds[[name(k)]] > 0)
      # if there aren't any, put the money in evenly across all
      if( is.null(growth.funds) ) growth.funds = 1:n
    }

    funds[[name(k)]][growth.funds] = funds[[name(k)]][growth.funds] + add.to.funds/length(growth.funds)
  }
  funds[['growth']] = cum.growth
  return(data.frame(funds))
}

