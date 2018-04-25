////////////////////////////////////////////////////////////////////////
//                     JS-CODE FOR ACTIVE TTB                         //
//                       AUTHOR: ERIC SCHULZ                          //
//                    UCL LONDON,  JANUARY 2014                       //
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
//INTIALIZE ARRAYS
////////////////////////////////////////////////////////////////////////
//Number of total trials
var ntrials = 29;
var ntrials2 = 9;

//index for array tracking
var index=0;
//dummy for trial length
var triallength = new Array(ntrials); // training 
var triallength2 = new Array(ntrials2); // testing: makes 10 array elements although ntrials2 = 9 I think (Array indexes start with 0 like in python)
//Alien1
var alien1 = new Array(ntrials);
//generated sun tracker
var alien2 = new Array(ntrials);
//generated rain tracker
var alien3 = new Array(ntrials);
//received reward tracker
var alienchosen = new Array(ntrials); //training: encodes which of two left Aliens was chosen


var alien1b = new Array(ntrials2);//  # alien1b and alien2b refer to the randomly chosen test aliens for the test phase (10 aliens)
//generated sun tracker
var alien2b = new Array(ntrials2); // # alien1b and alien2b refer to the randomly chosen test aliens for the test phase (10 aliens)
var triallength2 = new Array(ntrials2);


var tracker1 = new Array(ntrials); // correct answers on 30 learning trials: is it comptued with logistic regression on the fly 
var tracker2 = new Array(ntrials2); // correct answer on 10 test trials 
var weightsvector=[6.64992312,2.09378591,0.85354910,0.40274188]; // only theta = 1

//var chosenweight=Math.floor((Math.random() * 6) );
var weights=weightsvector;  // there is only one this time
weigths=perms(weights);

var myDataRef =  new Firebase('https://ttbactive2.firebaseio.com/');// defines the firebase url that data is pushed to



////////////////////////////////////////////////////////////////////////
//CREATE HELPER FUNCTIONS
////////////////////////////////////////////////////////////////////////
//hides page hide and shows page show
function clickStart(hide, show)  
{ 
        document.getElementById(hide).style.display ='none' ;
        document.getElementById(show).style.display ='block';
        window.scrollTo(0,0);        
}

//changes inner HTML of div with ID=x to y
function change (x,y){
    document.getElementById(x).innerHTML=y;
}

//Hides div with id=x
function hide(x){
  document.getElementById(x).style.display='none';
}

//shows div with id=x
function show(x){
  document.getElementById(x).style.display='block';
}

//sets a value at the end to hidden id
function setvalue(x,y){
  document.getElementById(x).value = y;
}

function sample(o){ 
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o[0];
};

function perms(o){ 
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};


//my norm generation
function myNorm() {
    var x1, x2, rad;
     do {
        x1  = 2 * Math.random() - 1;
        x2  = 2 * Math.random() - 1;
        rad = x1 * x1 + x2 * x2;
    } while(rad >= 1 || rad == 0);
     var c = Math.sqrt(-2 * Math.log(rad) / rad);
     return (x1 * c*5);
};

//Function to randomly shuffle an array and get first element
function generate(){ 
    var o=['0','1'];
    for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o[0];
};

function permute(o){
for(var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

// generates all the possible combinations that are there
function combinations(str) {   
    var fn = function(active, rest, a) {
        if (!active && !rest)
            return;
        if (!rest) {
            a.push(active);
        } else {
            fn(active + rest[0], rest.slice(1), a);
            fn(active, rest.slice(1), a);
        }
        return a;
    }
    return fn("", str, []);
}

var allcombs=combinations("10101010");  ///this has to be changed so that allcombs.length == 3

//  only appends those combinations from above that have length 4 (4 features) 
var combsa=[];
for (i = 0; i < allcombs.length; i++) {    
 if (allcombs[i].length===4) {combsa.push(allcombs[i]);} 
}

// filters out the doubles from all elements with length 4 
function unique(x){
   var u = {}, a = [];
   for(var i = 0, l = x.length; i < l; ++i){
      if(u.hasOwnProperty(x[i])) {
         continue;
      }
      a.push(x[i]);
      u[x[i]] = 1;
   }
   return a;
}

var combs=unique(combsa);   // check the list combsa for doubles. gives us our 16 unique Aliens.

// features samples 3 random Aliens from our 16 unique scrambled Aliens
function features (){  // function that takes no input
  var feature=new Array(3);
  combs=permute(combs);   // combs already has only 4 Aliens with unique feature combinations
for (i = 0; i < 3; i++) {  // changed from 0:4 to 0:3
  feature[i]=combs[i];
}
return(feature)
}


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
var all=features(); 
alien1[0]=all[0]; // is the comparison Alien on the top: random from above
alien2[0]=all[1];
alien3[0]=all[2];  


var myboarder=['border="1">', 'border="0">','border="0">']; // NOw in order of below. now there are only two which can have frames //default is 0
var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1[0]+'.png"  width="280" height="250"' + myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2[0]+'.png"  width="280" height="250" onclick="mymarkation(1)"' + myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien3[0]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];

// https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_0001.png // public link :)

change('a1', palien1); // now this is the compariosn Alien on top.
change('a2', palien2);
change('a3', palien3);  

// from test phase: only a1 and a2 can be clicked
var currentlychosen=-1; // other vlaue than 0 and 1
var mycounter=0;
mymarkation=function(which){
currentlychosen=which;  //z.b. 1 and 2 
if (currentlychosen===1){  // if the first one, 
  myboarder[1]='border="1">'
  myboarder[2]='border="0">'
  myboarder[0]='border="1">'

  } else{
    myboarder[2]='border="1">'  // make a frame (1) on the second element
    myboarder[1]='border="0">'
    myboarder[0]='border="1">'
  }
var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250"' + myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250" onclick="mymarkation(1)"' + myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];

change('a1', palien1);
change('a2', palien2);
change('a3', palien3);  // not sitting on top 

}



function compare(){
  if (currentlychosen!=-1){   // if it is different to "no selection" , ie. it will always just have 1 value in it
  a1=alien1[index];   // index starts at 0 (above) and then trial 
  a2=alien2[index];
  a3=alien3[index]; // 

var fullcollect=[a1,a2,a3];   // first one is now comparison alien (0)
var n1=currentlychosen;// either 1 or 2
var n2=0; // First array element always contains comparison alien!
var c1=fullcollect[n1]; // bottom alien
var c2=fullcollect[n2]; // always comparison Alien but random
var comparison =[n1,n2] // n1 = 1 or 2 or 0 and its always n1(c1) - n2(c2) below
alienchosen[index]=currentlychosen; //used to be 2 values ie 2 columns, with values 0,1,2,3. Now: one column with values 1 or 2
var compvec=[parseInt(c1.charAt(0))-parseInt(c2.charAt(0)),parseInt(c1.charAt(1))-parseInt(c2.charAt(1)),parseInt(c1.charAt(2))-parseInt(c2.charAt(2)),parseInt(c1.charAt(3))-parseInt(c2.charAt(3))];
var p=compvec[0]*weights[0]+compvec[1]*weights[1]+compvec[2]*weights[2]+compvec[3]*weights[3];
var prob=Math.exp(p)/(1+Math.exp(p)); // prob of y=c1 (bottom Alien)
var torealize=Math.round(prob*100);
var samplepool= new Array(100);    // makes it stochastic
for (var i = 0; i < 100; i++) {
   if (i<=torealize){samplepool[i]=0;}
   else{samplepool[i]=1;}
}  // 0 is now Alien on left/right, and 1 is alien on top


var outcome=sample(samplepool); // sample an outcome (0/1) with probability from above (i.e. 78 0s makes 0 more likely)
if (outcome===1){outcome2=0}
else{outcome2=1} // always the opposite
var winner=comparison[outcome]; // comparison[0] or comparison[1], so winner can contain values of 0, 1, 2
var loser=comparison[outcome2]; // comparion[1] or compariosn[0] exactly the opposite

myboarder=['border="1">', 'border="0">','border="0">']; // needs to be in same order as winner index, 0, 1,2.
myboarder[winner]='border="4" >';
myboarder[loser]='border="0.5" >';
var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250"'+ myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250"'+ myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250"'+ myboarder[2];

change('a1', palien1);
change('a2', palien2);
change('a3', palien3);

var outcometext="Alien "+(winner+1)+" has won the comparison." // 
clickStart('compare','next');  // displays the next trial button, and executes nextrrial function on click 
change('otcome',outcometext); // put the outcome test to the location of 'otcome'
tracker1[index]=winner+1; // will be 1, 2, or 3 now in order of above, left, right
}

else{alert("You have to choose one alien.")}
}

////////////////////////////////////////////////////////////////////////
//CREATE EXPERIMENTAL FUNCTIONS
////////////////////////////////////////////////////////////////////////


//overall trial function
function nexttrial(){
 if (triallength.length > 0) {  // as long as there are still more trials (before 0)
 //track trial number
  triallength.shift();  // shift one forward
  clickStart('explainshow','explainhide');  //track produced output
 
  //keep track for index used to assign array content
  index=index+1;
  var all=features();
  alien1[index]=all[0];
  alien2[index]=all[1];
  alien3[index]=all[2];


  var myboarder=['border="1">', 'border="0">','border="0">']; // Now in order of below. now there are only two which can have frames //default is 0
  var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250"' + myboarder[0];
  var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250" onclick="mymarkation(1)"' + myboarder[1];
  var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];

  change('a1', palien1);
  change('a2', palien2);
  change('a3', palien3);

  currentlychosen=-1;
  mycounter=0;
  clickStart('next','compare');   // this is where the actions is: nexttrial just displays next aliens, but compare html displays "Compete" Button, and on click executes compare() function above

  //show remaining number of trials
  var insert ='Number of trials left: '+(ntrials-index+1);
  change("remain",insert);
  //show total score
   //Last trial:
  if((ntrials-index)===0){
   //show "Go to next page button"
   change('nexttrial','Go to next page');
   //Show info that trials are done
  // change('finaltext','You have used up all of your trials. Please click on "Go to next page" to continue.');
  }
 }
 else {
   //go to next page
   clickStart('page4','page5');  // change from learning html to assessment html
 }
}



//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


var index2=0;
var all=features();
alien1b[0]=all[0];
alien2b[0]=all[1];
var myboarder2=['border="0">','border="0">'];
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1b[0]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2b[0]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];
change('a1b', palien1b);
change('a2b', palien2b);

var currentlychosen2=-1;
var mycounter2=0;
mymarkation2=function(which){
myboarder2=['border="0">','border="0">'];
currentlychosen2=which;
if (currentlychosen2===0){
  myboarder2[0]='border="1">'}
  else{
    myboarder2[1]='border="1">'
  }
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];

change('a1b', palien1b);
change('a2b', palien2b);
}

var totalscore=0;
function select(){
  a1b=alien1b[index2];
  a2b=alien2b[index2];
var fullcollect2=[a1b,a2b];
var n1b=currentlychosen2;
var n2b=1-n1b;
var c1b=fullcollect2[n1b];
var c2b=fullcollect2[n2b];
var compvecb=[parseInt(c1b.charAt(0))-parseInt(c2b.charAt(0)),parseInt(c1b.charAt(1))-parseInt(c2b.charAt(1)),parseInt(c1b.charAt(2))-parseInt(c2b.charAt(2)),parseInt(c1b.charAt(3))-parseInt(c2b.charAt(3))];
var p2=compvecb[0]*weights[0]+compvecb[1]*weights[1]+compvecb[2]*weights[2]+compvecb[3]*weights[3];
var prob2=Math.exp(p2)/(1+Math.exp(p2));
totalscore=totalscore+Math.round(prob2*100);
var winner2=currentlychosen2;
myboarder2=['border="0">','border="0">'];
myboarder2[winner2]='border="4" >';
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250"'+ myboarder2[1];
change('a1b', palien1b);
change('a2b', palien2b);
var outcometext2="You have chosen Alien "+(winner2+1);
tracker2[index2]=winner2+1;
clickStart('compare2','next2');
change('otcome2',outcometext2);
}
////////////////////////////////////////////////////////////////////////
//CREATE ASSESSMENT
////////////////////////////////////////////////////////////////////////


//overal trial function
function nexttrial2(){
 if (triallength2.length > 0) {
 //track trial number
  triallength2.shift();
  //track produced output
 
  //keep track for index used to assign array content
  index2=index2+1;
  var all=features();
  alien1b[index2]=all[0];
  alien2b[index2]=all[1];
  

  var myboarder2=['border="0">','border="0">'];
  var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
  var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/49780072/activettb/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];
  change('a1b', palien1b);
  change('a2b', palien2b);
  currentlychosen2=-1;
  mycounter2=0;
  clickStart('next2','compare2');
  //show remaining number of trials
  var insert2 ='Number of trials left: '+(ntrials2-index2+1);
  change("remain2",insert2);
  //show total score
   //Last trial:
  if((ntrials2-index2)===0){
   //show "Go to next page button"
   change('nexttrial2','Go to next page');
   //Show info that trials are done
   //change('finaltext','You have used up all of your trials. Please click on "Go to next page" to continue.');
  }
 }
 else {
   //go to next page
   clickStart('page5','page6');
 }
}
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


//Final submit function
function mysubmit()
{
 clickStart('page6','page7');
  //claculate number of mined emeralds overall
var age=90;
if (document.getElementById('age1').checked) {var  age = 20}
if (document.getElementById('age2').checked) {var  age = 30}
if (document.getElementById('age3').checked) {var  age = 40}
if (document.getElementById('age4').checked) {var  age = 50}

var gender=3;
if (document.getElementById('gender1').checked) {var  gender = 1}
if (document.getElementById('gender2').checked) {var  gender = 2}


 var id=document.getElementById("turkid").value;
 var presenttotal='Your chosen team consists of Aliens that would win '+totalscore/10+' % of the time.';
 //calculate money earned
 var money =Math.round(100*(0.5+totalscore/(150*100)*0.5))/100;
 var presentmoney='This equals a total reward of $'+money+'.';
 //show score and money
 change('result',presenttotal); change('money',presentmoney);
 //save all created values
 myDataRef.push({alien1: alien1, alien2: alien2, alien3: alien3,alien1b: alien1b, alien2b: alien2b, 
                 tracker1: tracker1, tracker2: tracker2, alienchosen: alienchosen, totalscore: totalscore, 
                 money: money, weights: weights, age: age, gender: gender, id: id});
 }
////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
