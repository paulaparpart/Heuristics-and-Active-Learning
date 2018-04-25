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
var triallength = new Array(ntrials);
var triallength2 = new Array(ntrials2);
//Alien1
var alien1 = new Array(ntrials);
//generated sun tracker
var alien2 = new Array(ntrials);
//generated rain tracker
var alien3 = new Array(ntrials);
//generated temperature tracker
var alien4 = new Array(ntrials);
//received reward tracker
var alienchosen = new Array(ntrials);
var alien1b = new Array(ntrials2);
//generated sun tracker
var alien2b = new Array(ntrials2);
var triallength2 = new Array(ntrials2);
var tracker1 = new Array(ntrials);
var tracker2 = new Array(ntrials2);
var weightsvector=[[10.00000000,0.00000000,0.00000000,0.00000000],[8.58442492,1.13264508,0.24517529,0.03775471],
                    [6.64992312,2.09378591,0.85354910,0.40274188],[4.67378888,2.50223227,1.70135106,1.12262779],
                    [2.5,2.5,2.5,2.5]];

var chosenweight=Math.floor((Math.random() * 6) );
var weights=weightsvector[chosenweight];
weigths=perms(weights);
var myDataRef = new Firebase('https://activettb.firebaseio.com/');

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

var allcombs=combinations("10101010");

var combsa=[];
for (i = 0; i < allcombs.length; i++) { 
 if (allcombs[i].length===4) {combsa.push(allcombs[i]);}
}

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

var combs=unique(combsa);

function features (){
  var feature=new Array(4);
  combs=permute(combs);
for (i = 0; i < 4; i++) { 
  feature[i]=combs[i];
}
return(feature)
}


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
var all=features();
alien1[0]=all[0];
alien2[0]=all[1];
alien3[0]=all[2];
alien4[0]=all[3];

var myboarder=['border="0">','border="0">','border="0">','border="0">'];

var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1[0]+'.png"  width="280" height="250" onclick="mymarkation(0)"' + myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2[0]+'.png"  width="280" height="250" onclick="mymarkation(1)"'+ myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien3[0]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];
var palien4='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien4[0]+'.png"  width="280" height="250" onclick="mymarkation(3)"'+ myboarder[3];

change('a1', palien1);
change('a2', palien2);
change('a3', palien3);
change('a4', palien4);

var currentlychosen=[0,-1];
var mycounter=0;
mymarkation=function(which){
currentlychosen[mycounter]=which;
mycounter=mycounter+1;
if (mycounter>1) {mycounter=0;}
 for (i = 0; i < 4; i++) { 
if (currentlychosen[0]===i || currentlychosen[1]==i){
  myboarder[i]='border="1">'}
  else{
    myboarder[i]='border="0">'
  }
}
var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250" onclick="mymarkation(0)"' + myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250" onclick="mymarkation(1)"'+ myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];
var palien4='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien4[index]+'.png"  width="280" height="250" onclick="mymarkation(3)"'+ myboarder[3];

change('a1', palien1);
change('a2', palien2);
change('a3', palien3);
change('a4', palien4);
}

function compare(){
  if (unique(currentlychosen).length>1){
  a1=alien1[index];
  a2=alien2[index];
  a3=alien3[index];
  a4=alien4[index];
var fullcollect=[a1,a2,a3,a4];
var n1=currentlychosen[0];
var n2=currentlychosen[1];
var c1=fullcollect[n1];
var c2=fullcollect[n2];
alienchosen[index]=currentlychosen;
var compvec=[parseInt(c1.charAt(0))-parseInt(c2.charAt(0)),parseInt(c1.charAt(1))-parseInt(c2.charAt(1)),parseInt(c1.charAt(2))-parseInt(c2.charAt(2)),parseInt(c1.charAt(3))-parseInt(c2.charAt(3))]
var p=compvec[0]*weights[0]+compvec[1]*weights[1]+compvec[2]*weights[2]+compvec[3]*weights[3];
var prob=Math.exp(p)/(1+Math.exp(p));
var torealize=Math.round(prob*100);
var samplepool= new Array(100);
for (var i = 0; i < 100; i++) {
   if (i<=torealize){samplepool[i]=0;}
   else{samplepool[i]=1;}
}

var outcome=sample(samplepool);
var winner=currentlychosen[outcome];
var loser=currentlychosen[!outcome];
myboarder=['border="0">','border="0">','border="0">','border="0">'];
myboarder[winner]='border="4" >';
myboarder[loser]='border="0.5" >';
var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250"'+ myboarder[0];
var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250"'+ myboarder[1];
var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250"'+ myboarder[2];
var palien4='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien4[index]+'.png"  width="280" height="250"'+ myboarder[3];

change('a1', palien1);
change('a2', palien2);
change('a3', palien3);
change('a4', palien4);
var outcometext="Alien "+(winner+1)+" has won the comparison."
clickStart('compare','next');
change('otcome',outcometext);
tracker1[index]=winner+1;}
else{alert("You have to choose two aliens.")}
}
////////////////////////////////////////////////////////////////////////
//CREATE EXPERIMENTAL FUNCTIONS
////////////////////////////////////////////////////////////////////////


//overall trial function
function nexttrial(){
 if (triallength.length > 0) {
 //track trial number
  triallength.shift();
  //track produced output
 
  //keep track for index used to assign array content
  index=index+1;
  var all=features();
  alien1[index]=all[0];
  alien2[index]=all[1];
  alien3[index]=all[2];
  alien4[index]=all[3];

  var myboarder=['border="0">','border="0">','border="0">','border="0">'];
  var palien1='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1[index]+'.png"  width="280" height="250" onclick="mymarkation(0)"' + myboarder[0];
  var palien2='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2[index]+'.png"  width="280" height="250" onclick="mymarkation(1)"'+ myboarder[1];
  var palien3='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien3[index]+'.png"  width="280" height="250" onclick="mymarkation(2)"'+ myboarder[2];
  var palien4='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien4[index]+'.png"  width="280" height="250" onclick="mymarkation(3)"'+ myboarder[3];
  change('a1', palien1);
  change('a2', palien2);
  change('a3', palien3);
  change('a4', palien4);
  currentlychosen=[-1,-1];
  mycounter=0;
  clickStart('next','compare');
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
   clickStart('page4','page5');
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
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1b[0]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2b[0]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];
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
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];

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
var compvecb=[parseInt(c1b.charAt(0))-parseInt(c2b.charAt(0)),parseInt(c1b.charAt(1))-parseInt(c2b.charAt(1)),parseInt(c1b.charAt(2))-parseInt(c2b.charAt(2)),parseInt(c1b.charAt(3))-parseInt(c2b.charAt(3))]
var p2=compvecb[0]*weights[0]+compvecb[1]*weights[1]+compvecb[2]*weights[2]+compvecb[3]*weights[3];
var prob2=Math.exp(p2)/(1+Math.exp(p2));
totalscore=totalscore+Math.round(prob2*100);
var winner2=currentlychosen2;
myboarder2=['border="0">','border="0">'];
myboarder2[winner2]='border="4" >';
var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250"' + myboarder2[0];
var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250"'+ myboarder2[1];
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
  var palien1b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien1b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(0)"' + myboarder2[0];
  var palien2b='<input type="image" src="https://dl.dropboxusercontent.com/u/4213788/Aliens/Alien_'+alien2b[index2]+'.png"  width="280" height="250" onclick="mymarkation2(1)"'+ myboarder2[1];
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


var gender=document.getElementById("gender").value;
var age=document.getElementById("age").value;
//Final submit function
function mysubmit(){
  clickStart('page6','page7');
  //claculate number of mined emeralds overall
 var presenttotal='Your chosen team consists of Aliens that would win '+totalscore/10+' % of the times.';
 //calculate money earned
 var money =Math.round(100*(0.5+totalscore/(150*100)*0.5))/100;
 var presentmoney='This equals a total reward of $ '+money+'.';
 //show score and money
 change('result',presenttotal); change('money',presentmoney);
 //save all created values
 myDataRef.push({alien1: alien1, alien2: alien2, alien3: alien3, alien4: alien4,alien1b: alien1b, alien2b: alien2b, 
                 tracker1: tracker1, tracker2: tracker2, alienchosen: alienchosen, totalscore: totalscore, weights: weights,
                 age: age, gender: gender});
 }
////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
