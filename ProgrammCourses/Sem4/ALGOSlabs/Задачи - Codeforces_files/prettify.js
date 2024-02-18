var DecorationsT;var JobT;var SourceSpansT;var IN_GLOBAL_SCOPE=true;var PR;var PR_SHOULD_USE_CONTINUATION=true
if(typeof window!=='undefined'){window['PR_SHOULD_USE_CONTINUATION']=PR_SHOULD_USE_CONTINUATION;}
var prettyPrintOne;var prettyPrint;(function(){var win=(typeof window!=='undefined')?window:{};var FLOW_CONTROL_KEYWORDS=["break,continue,do,else,for,forn,if,return,while"];var C_KEYWORDS=[FLOW_CONTROL_KEYWORDS,"auto,case,char,const,default,"+
"double,enum,extern,float,goto,inline,int,long,register,restrict,short,signed,"+
"sizeof,static,struct,switch,typedef,union,unsigned,void,volatile"];var COMMON_KEYWORDS=[C_KEYWORDS,"catch,class,delete,false,import,"+
"new,operator,private,protected,public,this,throw,true,try,typeof"];var CPP_KEYWORDS=[COMMON_KEYWORDS,"alignas,alignof,align_union,asm,axiom,bool,"+
"concept,concept_map,const_cast,constexpr,decltype,delegate,"+
"dynamic_cast,explicit,export,friend,generic,late_check,"+
"mutable,namespace,noexcept,noreturn,nullptr,property,reinterpret_cast,static_assert,"+
"static_cast,template,typeid,typename,using,virtual,where"];var JAVA_KEYWORDS=[COMMON_KEYWORDS,"abstract,assert,boolean,byte,extends,finally,final,implements,import,"+
"instanceof,interface,null,native,package,strictfp,super,synchronized,"+
"throws,transient"];var CSHARP_KEYWORDS=[COMMON_KEYWORDS,"abstract,add,alias,as,ascending,async,await,base,bool,by,byte,checked,decimal,delegate,descending,"+
"dynamic,event,finally,fixed,foreach,from,get,global,group,implicit,in,interface,"+
"internal,into,is,join,let,lock,null,object,out,override,orderby,params,"+
"partial,readonly,ref,remove,sbyte,sealed,select,set,stackalloc,string,select,uint,ulong,"+
"unchecked,unsafe,ushort,value,var,virtual,where,yield"];var COFFEE_KEYWORDS="all,and,by,catch,class,else,extends,false,finally,"+
"for,if,in,is,isnt,loop,new,no,not,null,of,off,on,or,return,super,then,"+
"throw,true,try,unless,until,when,while,yes";var JSCRIPT_KEYWORDS=[COMMON_KEYWORDS,"abstract,async,await,constructor,debugger,enum,eval,export,from,function,"+
"get,import,implements,instanceof,interface,let,null,of,set,undefined,"+
"var,with,yield,Infinity,NaN"];var PERL_KEYWORDS="caller,delete,die,do,dump,elsif,eval,exit,foreach,for,"+
"goto,if,import,last,local,my,next,no,our,print,package,redo,require,"+
"sub,undef,unless,until,use,wantarray,while,BEGIN,END";var PYTHON_KEYWORDS=[FLOW_CONTROL_KEYWORDS,"and,as,assert,class,def,del,"+
"elif,except,exec,finally,from,global,import,in,is,lambda,"+
"nonlocal,not,or,pass,print,raise,try,with,yield,"+
"False,True,None"];var RUBY_KEYWORDS=[FLOW_CONTROL_KEYWORDS,"alias,and,begin,case,class,"+
"def,defined,elsif,end,ensure,false,in,module,next,nil,not,or,redo,"+
"rescue,retry,self,super,then,true,undef,unless,until,when,yield,"+
"BEGIN,END"];var SH_KEYWORDS=[FLOW_CONTROL_KEYWORDS,"case,done,elif,esac,eval,fi,"+
"function,in,local,set,then,until"];var ALL_KEYWORDS=[CPP_KEYWORDS,CSHARP_KEYWORDS,JAVA_KEYWORDS,JSCRIPT_KEYWORDS,PERL_KEYWORDS,PYTHON_KEYWORDS,RUBY_KEYWORDS,SH_KEYWORDS];var C_TYPES=/^(DIR|FILE|array|vector|(de|priority_)?queue|(forward_)?list|stack|(const_)?(reverse_)?iterator|(unordered_)?(multi)?(set|map)|bitset|u?(int|float)\d*)\b/;var PR_STRING='str';var PR_KEYWORD='kwd';var PR_COMMENT='com';var PR_TYPE='typ';var PR_LITERAL='lit';var PR_PUNCTUATION='pun';var PR_PLAIN='pln';var PR_TAG='tag';var PR_DECLARATION='dec';var PR_SOURCE='src';var PR_ATTRIB_NAME='atn';var PR_ATTRIB_VALUE='atv';var PR_NOCODE='nocode';var REGEXP_PRECEDER_PATTERN='(?:^^\\.?|[+-]|[!=]=?=?|\\#|%=?|&&?=?|\\(|\\*=?|[+\\-]=|->|\\/=?|::?|<<?=?|>>?>?=?|,|;|\\?|@|\\[|~|{|\\^\\^?=?|\\|\\|?=?|break|case|continue|delete|do|else|finally|instanceof|return|throw|try|typeof)\\s*';function combinePrefixPatterns(regexs){var capturedGroupIndex=0;var needToFoldCase=false;var ignoreCase=false;for(var i=0,n=regexs.length;i<n;++i){var regex=regexs[i];if(regex.ignoreCase){ignoreCase=true;}else if(/[a-z]/i.test(regex.source.replace(/\\u[0-9a-f]{4}|\\x[0-9a-f]{2}|\\[^ux]/gi,''))){needToFoldCase=true;ignoreCase=false;break;}}
var escapeCharToCodeUnit={'b':8,'t':9,'n':0xa,'v':0xb,'f':0xc,'r':0xd};function decodeEscape(charsetPart){var cc0=charsetPart.charCodeAt(0);if(cc0!==92){return cc0;}
var c1=charsetPart.charAt(1);cc0=escapeCharToCodeUnit[c1];if(cc0){return cc0;}else if('0'<=c1&&c1<='7'){return parseInt(charsetPart.substring(1),8);}else if(c1==='u'||c1==='x'){return parseInt(charsetPart.substring(2),16);}else{return charsetPart.charCodeAt(1);}}
function encodeEscape(charCode){if(charCode<0x20){return(charCode<0x10?'\\x0':'\\x')+charCode.toString(16);}
var ch=String.fromCharCode(charCode);return(ch==='\\'||ch==='-'||ch===']'||ch==='^')?"\\"+ch:ch;}
function caseFoldCharset(charSet){var charsetParts=charSet.substring(1,charSet.length-1).match(new RegExp('\\\\u[0-9A-Fa-f]{4}'
+'|\\\\x[0-9A-Fa-f]{2}'
+'|\\\\[0-3][0-7]{0,2}'
+'|\\\\[0-7]{1,2}'
+'|\\\\[\\s\\S]'
+'|-'
+'|[^-\\\\]','g'));var ranges=[];var inverse=charsetParts[0]==='^';var out=['['];if(inverse){out.push('^');}
for(var i=inverse?1:0,n=charsetParts.length;i<n;++i){var p=charsetParts[i];if(/\\[bdsw]/i.test(p)){out.push(p);}else{var start=decodeEscape(p);var end;if(i+2<n&&'-'===charsetParts[i+1]){end=decodeEscape(charsetParts[i+2]);i+=2;}else{end=start;}
ranges.push([start,end]);if(!(end<65||start>122)){if(!(end<65||start>90)){ranges.push([Math.max(65,start)|32,Math.min(end,90)|32]);}
if(!(end<97||start>122)){ranges.push([Math.max(97,start)&~32,Math.min(end,122)&~32]);}}}}
ranges.sort(function(a,b){return(a[0]-b[0])||(b[1]-a[1]);});var consolidatedRanges=[];var lastRange=[];for(var i=0;i<ranges.length;++i){var range=ranges[i];if(range[0]<=lastRange[1]+1){lastRange[1]=Math.max(lastRange[1],range[1]);}else{consolidatedRanges.push(lastRange=range);}}
for(var i=0;i<consolidatedRanges.length;++i){var range=consolidatedRanges[i];out.push(encodeEscape(range[0]));if(range[1]>range[0]){if(range[1]+1>range[0]){out.push('-');}
out.push(encodeEscape(range[1]));}}
out.push(']');return out.join('');}
function allowAnywhereFoldCaseAndRenumberGroups(regex){var parts=regex.source.match(new RegExp('(?:'
+'\\[(?:[^\\x5C\\x5D]|\\\\[\\s\\S])*\\]'
+'|\\\\u[A-Fa-f0-9]{4}'
+'|\\\\x[A-Fa-f0-9]{2}'
+'|\\\\[0-9]+'
+'|\\\\[^ux0-9]'
+'|\\(\\?[:!=]'
+'|[\\(\\)\\^]'
+'|[^\\x5B\\x5C\\(\\)\\^]+'
+')','g'));var n=parts.length;var capturedGroups=[];for(var i=0,groupIndex=0;i<n;++i){var p=parts[i];if(p==='('){++groupIndex;}else if('\\'===p.charAt(0)){var decimalValue=+p.substring(1);if(decimalValue){if(decimalValue<=groupIndex){capturedGroups[decimalValue]=-1;}else{parts[i]=encodeEscape(decimalValue);}}}}
for(var i=1;i<capturedGroups.length;++i){if(-1===capturedGroups[i]){capturedGroups[i]=++capturedGroupIndex;}}
for(var i=0,groupIndex=0;i<n;++i){var p=parts[i];if(p==='('){++groupIndex;if(!capturedGroups[groupIndex]){parts[i]='(?:';}}else if('\\'===p.charAt(0)){var decimalValue=+p.substring(1);if(decimalValue&&decimalValue<=groupIndex){parts[i]='\\'+capturedGroups[decimalValue];}}}
for(var i=0;i<n;++i){if('^'===parts[i]&&'^'!==parts[i+1]){parts[i]='';}}
if(regex.ignoreCase&&needToFoldCase){for(var i=0;i<n;++i){var p=parts[i];var ch0=p.charAt(0);if(p.length>=2&&ch0==='['){parts[i]=caseFoldCharset(p);}else if(ch0!=='\\'){parts[i]=p.replace(/[a-zA-Z]/g,function(ch){var cc=ch.charCodeAt(0);return '['+String.fromCharCode(cc&~32,cc|32)+']';});}}}
return parts.join('');}
var rewritten=[];for(var i=0,n=regexs.length;i<n;++i){var regex=regexs[i];if(regex.global||regex.multiline){throw new Error(''+regex);}
rewritten.push('(?:'+allowAnywhereFoldCaseAndRenumberGroups(regex)+')');}
return new RegExp(rewritten.join('|'),ignoreCase?'gi':'g');}
function extractSourceSpans(node,isPreformatted){var nocode=/(?:^|\s)nocode(?:\s|$)/;var chunks=[];var length=0;var spans=[];var k=0;function walk(node){var type=node.nodeType;if(type==1){if(nocode.test(node.className)){return;}
for(var child=node.firstChild;child;child=child.nextSibling){walk(child);}
var nodeName=node.nodeName.toLowerCase();if('br'===nodeName||'li'===nodeName){chunks[k]='\n';spans[k<<1]=length++;spans[(k++<<1)|1]=node;}}else if(type==3||type==4){var text=node.nodeValue;if(text.length){if(!isPreformatted){text=text.replace(/[ \t\r\n]+/g,' ');}else{text=text.replace(/\r\n?/g,'\n');}
chunks[k]=text;spans[k<<1]=length;length+=text.length;spans[(k++<<1)|1]=node;}}}
walk(node);return{sourceCode:chunks.join('').replace(/\n$/,''),spans:spans};}
function appendDecorations(sourceNode,basePos,sourceCode,langHandler,out){if(!sourceCode){return;}
var job={sourceNode:sourceNode,pre:1,langExtension:null,numberLines:null,sourceCode:sourceCode,spans:null,basePos:basePos,decorations:null};langHandler(job);out.push.apply(out,job.decorations);}
var notWs=/\S/;function childContentWrapper(element){var wrapper=undefined;for(var c=element.firstChild;c;c=c.nextSibling){var type=c.nodeType;wrapper=(type===1)?(wrapper?element:c):(type===3)?(notWs.test(c.nodeValue)?element:wrapper):wrapper;}
return wrapper===element?undefined:wrapper;}
function createSimpleLexer(shortcutStylePatterns,fallthroughStylePatterns){var shortcuts={};var tokenizer;(function(){var allPatterns=shortcutStylePatterns.concat(fallthroughStylePatterns);var allRegexs=[];var regexKeys={};for(var i=0,n=allPatterns.length;i<n;++i){var patternParts=allPatterns[i];var shortcutChars=patternParts[3];if(shortcutChars){for(var c=shortcutChars.length;--c>=0;){shortcuts[shortcutChars.charAt(c)]=patternParts;}}
var regex=patternParts[1];var k=''+regex;if(!regexKeys.hasOwnProperty(k)){allRegexs.push(regex);regexKeys[k]=null;}}
allRegexs.push(/[\0-\uffff]/);tokenizer=combinePrefixPatterns(allRegexs);})();var nPatterns=fallthroughStylePatterns.length;var decorate=function(job){var sourceCode=job.sourceCode,basePos=job.basePos;var sourceNode=job.sourceNode;var decorations=[basePos,PR_PLAIN];var pos=0;var tokens=sourceCode.match(tokenizer)||[];var styleCache={};for(var ti=0,nTokens=tokens.length;ti<nTokens;++ti){var token=tokens[ti];var style=styleCache[token];var match=void 0;var isEmbedded;if(typeof style==='string'){isEmbedded=false;}else{var patternParts=shortcuts[token.charAt(0)];if(patternParts){match=token.match(patternParts[1]);style=patternParts[0];}else{for(var i=0;i<nPatterns;++i){patternParts=fallthroughStylePatterns[i];match=token.match(patternParts[1]);if(match){style=patternParts[0];break;}}
if(!match){style=PR_PLAIN;}}
isEmbedded=style.length>=5&&'lang-'===style.substring(0,5);if(isEmbedded&&!(match&&typeof match[1]==='string')){isEmbedded=false;style=PR_SOURCE;}
if(!isEmbedded){styleCache[token]=style;}}
var tokenStart=pos;pos+=token.length;if(!isEmbedded){decorations.push(basePos+tokenStart,style);}else{var embeddedSource=match[1];var embeddedSourceStart=token.indexOf(embeddedSource);var embeddedSourceEnd=embeddedSourceStart+embeddedSource.length;if(match[2]){embeddedSourceEnd=token.length-match[2].length;embeddedSourceStart=embeddedSourceEnd-embeddedSource.length;}
var lang=style.substring(5);appendDecorations(sourceNode,basePos+tokenStart,token.substring(0,embeddedSourceStart),decorate,decorations);appendDecorations(sourceNode,basePos+tokenStart+embeddedSourceStart,embeddedSource,langHandlerForExtension(lang,embeddedSource),decorations);appendDecorations(sourceNode,basePos+tokenStart+embeddedSourceEnd,token.substring(embeddedSourceEnd),decorate,decorations);}}
job.decorations=decorations;};return decorate;}
function sourceDecorator(options){var shortcutStylePatterns=[],fallthroughStylePatterns=[];if(options['tripleQuotedStrings']){shortcutStylePatterns.push([PR_STRING,/^(?:\'\'\'(?:[^\'\\]|\\[\s\S]|\'{1,2}(?=[^\']))*(?:\'\'\'|$)|\"\"\"(?:[^\"\\]|\\[\s\S]|\"{1,2}(?=[^\"]))*(?:\"\"\"|$)|\'(?:[^\\\']|\\[\s\S])*(?:\'|$)|\"(?:[^\\\"]|\\[\s\S])*(?:\"|$))/,null,'\'"']);}else if(options['multiLineStrings']){shortcutStylePatterns.push([PR_STRING,/^(?:\'(?:[^\\\']|\\[\s\S])*(?:\'|$)|\"(?:[^\\\"]|\\[\s\S])*(?:\"|$)|\`(?:[^\\\`]|\\[\s\S])*(?:\`|$))/,null,'\'"`']);}else{shortcutStylePatterns.push([PR_STRING,/^(?:\'(?:[^\\\'\r\n]|\\.)*(?:\'|$)|\"(?:[^\\\"\r\n]|\\.)*(?:\"|$))/,null,'"\'']);}
if(options['verbatimStrings']){fallthroughStylePatterns.push([PR_STRING,/^@\"(?:[^\"]|\"\")*(?:\"|$)/,null]);}
var hc=options['hashComments'];if(hc){if(options['cStyleComments']){if(hc>1){shortcutStylePatterns.push([PR_COMMENT,/^#(?:##(?:[^#]|#(?!##))*(?:###|$)|.*)/,null,'#']);}else{shortcutStylePatterns.push([PR_COMMENT,/^#(?:(?:define|e(?:l|nd)if|else|error|ifn?def|include|line|pragma|undef|warning)\b|[^\r\n]*)/,null,'#']);}
fallthroughStylePatterns.push([PR_STRING,/^<(?:(?:(?:\.\.\/)*|\/?)(?:[\w-]+(?:\/[\w-]+)+)?[\w-]+\.h(?:h|pp|\+\+)?|[a-z]\w*)>/,null]);}else{shortcutStylePatterns.push([PR_COMMENT,/^#[^\r\n]*/,null,'#']);}}
if(options['cStyleComments']){fallthroughStylePatterns.push([PR_COMMENT,/^\/\/[^\r\n]*/,null]);fallthroughStylePatterns.push([PR_COMMENT,/^\/\*[\s\S]*?(?:\*\/|$)/,null]);}
var regexLiterals=options['regexLiterals'];if(regexLiterals){var regexExcls=regexLiterals>1?'':'\n\r';var regexAny=regexExcls?'.':'[\\S\\s]';var REGEX_LITERAL=('/(?=[^/*'+regexExcls+'])'
+'(?:[^/\\x5B\\x5C'+regexExcls+']'
+'|\\x5C'+regexAny
+'|\\x5B(?:[^\\x5C\\x5D'+regexExcls+']'
+'|\\x5C'+regexAny+')*(?:\\x5D|$))+'
+'/');fallthroughStylePatterns.push(['lang-regex',RegExp('^'+REGEXP_PRECEDER_PATTERN+'('+REGEX_LITERAL+')')]);}
var types=options['types'];if(types){fallthroughStylePatterns.push([PR_TYPE,types]);}
var keywords=(""+options['keywords']).replace(/^ | $/g,'');if(keywords.length){fallthroughStylePatterns.push([PR_KEYWORD,new RegExp('^(?:'+keywords.replace(/[\s,]+/g,'|')+')\\b'),null]);}
shortcutStylePatterns.push([PR_PLAIN,/^\s+/,null,' \r\n\t\xA0']);var punctuation='^.[^\\s\\w.$@\'"`/\\\\]*';if(options['regexLiterals']){punctuation+='(?!\s*\/)';}
fallthroughStylePatterns.push([PR_LITERAL,/^@[a-z_$][a-z_$@0-9]*/i,null],[PR_TYPE,/^(?:[@_]?[A-Z]+[a-z][A-Za-z_$@0-9]*|\w+_t\b)/,null],[PR_PLAIN,/^[a-z_$][a-z_$@0-9]*/i,null],[PR_LITERAL,new RegExp('^(?:'
+'0x[a-f0-9]+'
+'|(?:\\d(?:_\\d+)*\\d*(?:\\.\\d*)?|\\.\\d\\+)'
+'(?:e[+\\-]?\\d+)?'
+')'
+'[a-z]*','i'),null,'0123456789'],[PR_PLAIN,/^\\[\s\S]?/,null],[PR_PUNCTUATION,new RegExp(punctuation),null]);return createSimpleLexer(shortcutStylePatterns,fallthroughStylePatterns);}
var decorateSource=sourceDecorator({'keywords':ALL_KEYWORDS,'hashComments':true,'cStyleComments':true,'multiLineStrings':true,'regexLiterals':true});function numberLines(node,startLineNum,isPreformatted){var nocode=/(?:^|\s)nocode(?:\s|$)/;var lineBreak=/\r\n?|\n/;var document=node.ownerDocument;var li=document.createElement('li');while(node.firstChild){li.appendChild(node.firstChild);}
var listItems=[li];function walk(node){var type=node.nodeType;if(type==1&&!nocode.test(node.className)){if('br'===node.nodeName.toLowerCase()){breakAfter(node);if(node.parentNode){node.parentNode.removeChild(node);}}else{for(var child=node.firstChild;child;child=child.nextSibling){walk(child);}}}else if((type==3||type==4)&&isPreformatted){var text=node.nodeValue;var match=text.match(lineBreak);if(match){var firstLine=text.substring(0,match.index);node.nodeValue=firstLine;var tail=text.substring(match.index+match[0].length);if(tail){var parent=node.parentNode;parent.insertBefore(document.createTextNode(tail),node.nextSibling);}
breakAfter(node);if(!firstLine){node.parentNode.removeChild(node);}}}}
function breakAfter(lineEndNode){while(!lineEndNode.nextSibling){lineEndNode=lineEndNode.parentNode;if(!lineEndNode){return;}}
function breakLeftOf(limit,copy){var rightSide=copy?limit.cloneNode(false):limit;var parent=limit.parentNode;if(parent){var parentClone=breakLeftOf(parent,1);var next=limit.nextSibling;parentClone.appendChild(rightSide);for(var sibling=next;sibling;sibling=next){next=sibling.nextSibling;parentClone.appendChild(sibling);}}
return rightSide;}
var copiedListItem=breakLeftOf(lineEndNode.nextSibling,0);for(var parent;(parent=copiedListItem.parentNode)&&parent.nodeType===1;){copiedListItem=parent;}
listItems.push(copiedListItem);}
for(var i=0;i<listItems.length;++i){walk(listItems[i]);}
if(startLineNum===(startLineNum|0)){listItems[0].setAttribute('value',startLineNum);}
var ol=document.createElement('ol');ol.className='linenums';var offset=Math.max(0,((startLineNum-1))|0)||0;for(var i=0,n=listItems.length;i<n;++i){li=listItems[i];li.className='L'+((i+offset)%10);if(!li.firstChild){li.appendChild(document.createTextNode('\xA0'));}
ol.appendChild(li);}
node.appendChild(ol);}
function recombineTagsAndDecorations(job){var isIE8OrEarlier=/\bMSIE\s(\d+)/.exec(navigator.userAgent);isIE8OrEarlier=isIE8OrEarlier&&+isIE8OrEarlier[1]<=8;var newlineRe=/\n/g;var source=job.sourceCode;var sourceLength=source.length;var sourceIndex=0;var spans=job.spans;var nSpans=spans.length;var spanIndex=0;var decorations=job.decorations;var nDecorations=decorations.length;var decorationIndex=0;decorations[nDecorations]=sourceLength;var decPos,i;for(i=decPos=0;i<nDecorations;){if(decorations[i]!==decorations[i+2]){decorations[decPos++]=decorations[i++];decorations[decPos++]=decorations[i++];}else{i+=2;}}
nDecorations=decPos;for(i=decPos=0;i<nDecorations;){var startPos=decorations[i];var startDec=decorations[i+1];var end=i+2;while(end+2<=nDecorations&&decorations[end+1]===startDec){end+=2;}
decorations[decPos++]=startPos;decorations[decPos++]=startDec;i=end;}
nDecorations=decorations.length=decPos;var sourceNode=job.sourceNode;var oldDisplay="";if(sourceNode){oldDisplay=sourceNode.style.display;sourceNode.style.display='none';}
try{var decoration=null;while(spanIndex<nSpans){var spanStart=spans[spanIndex];var spanEnd=(spans[spanIndex+2])||sourceLength;var decEnd=decorations[decorationIndex+2]||sourceLength;var end=Math.min(spanEnd,decEnd);var textNode=(spans[spanIndex+1]);var styledText;if(textNode.nodeType!==1&&(styledText=source.substring(sourceIndex,end))){if(isIE8OrEarlier){styledText=styledText.replace(newlineRe,'\r');}
textNode.nodeValue=styledText;var document=textNode.ownerDocument;var span=document.createElement('span');span.className=decorations[decorationIndex+1];var parentNode=textNode.parentNode;parentNode.replaceChild(span,textNode);span.appendChild(textNode);if(sourceIndex<spanEnd){spans[spanIndex+1]=textNode=document.createTextNode(source.substring(end,spanEnd));parentNode.insertBefore(textNode,span.nextSibling);}}
sourceIndex=end;if(sourceIndex>=spanEnd){spanIndex+=2;}
if(sourceIndex>=decEnd){decorationIndex+=2;}}}finally{if(sourceNode){sourceNode.style.display=oldDisplay;}}}
var langHandlerRegistry={};function registerLangHandler(handler,fileExtensions){for(var i=fileExtensions.length;--i>=0;){var ext=fileExtensions[i];if(!langHandlerRegistry.hasOwnProperty(ext)){langHandlerRegistry[ext]=handler;}else if(win['console']){console['warn']('cannot override language handler %s',ext);}}}
function langHandlerForExtension(extension,source){if(!(extension&&langHandlerRegistry.hasOwnProperty(extension))){extension=/^\s*</.test(source)?'default-markup':'default-code';}
return langHandlerRegistry[extension];}
registerLangHandler(decorateSource,['default-code']);registerLangHandler(createSimpleLexer([],[[PR_PLAIN,/^[^<?]+/],[PR_DECLARATION,/^<!\w[^>]*(?:>|$)/],[PR_COMMENT,/^<\!--[\s\S]*?(?:-\->|$)/],['lang-',/^<\?([\s\S]+?)(?:\?>|$)/],['lang-',/^<%([\s\S]+?)(?:%>|$)/],[PR_PUNCTUATION,/^(?:<[%?]|[%?]>)/],['lang-',/^<xmp\b[^>]*>([\s\S]+?)<\/xmp\b[^>]*>/i],['lang-js',/^<script\b[^>]*>([\s\S]*?)(<\/script\b[^>]*>)/i],['lang-css',/^<style\b[^>]*>([\s\S]*?)(<\/style\b[^>]*>)/i],['lang-in.tag',/^(<\/?[a-z][^<>]*>)/i]]),['default-markup','htm','html','mxml','xhtml','xml','xsl']);registerLangHandler(createSimpleLexer([[PR_PLAIN,/^[\s]+/,null,' \t\r\n'],[PR_ATTRIB_VALUE,/^(?:\"[^\"]*\"?|\'[^\']*\'?)/,null,'\"\'']],[[PR_TAG,/^^<\/?[a-z](?:[\w.:-]*\w)?|\/?>$/i],[PR_ATTRIB_NAME,/^(?!style[\s=]|on)[a-z](?:[\w:-]*\w)?/i],['lang-uq.val',/^=\s*([^>\'\"\s]*(?:[^>\'\"\s\/]|\/(?=\s)))/],[PR_PUNCTUATION,/^[=<>\/]+/],['lang-js',/^on\w+\s*=\s*\"([^\"]+)\"/i],['lang-js',/^on\w+\s*=\s*\'([^\']+)\'/i],['lang-js',/^on\w+\s*=\s*([^\"\'>\s]+)/i],['lang-css',/^style\s*=\s*\"([^\"]+)\"/i],['lang-css',/^style\s*=\s*\'([^\']+)\'/i],['lang-css',/^style\s*=\s*([^\"\'>\s]+)/i]]),['in.tag']);registerLangHandler(createSimpleLexer([],[[PR_ATTRIB_VALUE,/^[\s\S]+/]]),['uq.val']);registerLangHandler(sourceDecorator({'keywords':CPP_KEYWORDS,'hashComments':true,'cStyleComments':true,'types':C_TYPES}),['c','cc','cpp','cxx','cyc','m']);registerLangHandler(sourceDecorator({'keywords':'null,true,false'}),['json']);registerLangHandler(sourceDecorator({'keywords':CSHARP_KEYWORDS,'hashComments':true,'cStyleComments':true,'verbatimStrings':true,'types':C_TYPES}),['cs']);registerLangHandler(sourceDecorator({'keywords':JAVA_KEYWORDS,'cStyleComments':true}),['java']);registerLangHandler(sourceDecorator({'keywords':SH_KEYWORDS,'hashComments':true,'multiLineStrings':true}),['bash','bsh','csh','sh']);registerLangHandler(sourceDecorator({'keywords':PYTHON_KEYWORDS,'hashComments':true,'multiLineStrings':true,'tripleQuotedStrings':true}),['cv','py','python']);registerLangHandler(sourceDecorator({'keywords':PERL_KEYWORDS,'hashComments':true,'multiLineStrings':true,'regexLiterals':2}),['perl','pl','pm']);registerLangHandler(sourceDecorator({'keywords':RUBY_KEYWORDS,'hashComments':true,'multiLineStrings':true,'regexLiterals':true}),['rb','ruby']);registerLangHandler(sourceDecorator({'keywords':JSCRIPT_KEYWORDS,'cStyleComments':true,'regexLiterals':true}),['javascript','js','ts','typescript']);registerLangHandler(sourceDecorator({'keywords':COFFEE_KEYWORDS,'hashComments':3,'cStyleComments':true,'multilineStrings':true,'tripleQuotedStrings':true,'regexLiterals':true}),['coffee']);registerLangHandler(createSimpleLexer([],[[PR_STRING,/^[\s\S]+/]]),['regex']);function applyDecorator(job){var opt_langExtension=job.langExtension;try{var sourceAndSpans=extractSourceSpans(job.sourceNode,job.pre);var source=sourceAndSpans.sourceCode;job.sourceCode=source;job.spans=sourceAndSpans.spans;job.basePos=0;langHandlerForExtension(opt_langExtension,source)(job);recombineTagsAndDecorations(job);}catch(e){if(win['console']){console['log'](e&&e['stack']||e);}}}
function $prettyPrintOne(sourceCodeHtml,opt_langExtension,opt_numberLines){var nl=opt_numberLines||false;var langExtension=opt_langExtension||null;var container=document.createElement('div');container.innerHTML='<pre>'+sourceCodeHtml+'</pre>';container=(container.firstChild);if(nl){numberLines(container,nl,true);}
var job={langExtension:langExtension,numberLines:nl,sourceNode:container,pre:1,sourceCode:null,basePos:null,spans:null,decorations:null};applyDecorator(job);return container.innerHTML;}
function $prettyPrint(opt_whenDone,opt_root){var root=opt_root||document.body;var doc=root.ownerDocument||document;function byTagName(tn){return root.getElementsByTagName(tn);}
var codeSegments=[byTagName('pre'),byTagName('code'),byTagName('xmp')];var elements=[];for(var i=0;i<codeSegments.length;++i){for(var j=0,n=codeSegments[i].length;j<n;++j){elements.push(codeSegments[i][j]);}}
codeSegments=null;var clock=Date;if(!clock['now']){clock={'now':function(){return+(new Date);}};}
var k=0;var langExtensionRe=/\blang(?:uage)?-([\w.]+)(?!\S)/;var prettyPrintRe=/\bprettyprint\b/;var prettyPrintedRe=/\bprettyprinted\b/;var preformattedTagNameRe=/pre|xmp/i;var codeRe=/^code$/i;var preCodeXmpRe=/^(?:pre|code|xmp)$/i;var EMPTY={};function doWork(){var endTime=(win['PR_SHOULD_USE_CONTINUATION']?clock['now']()+250:Infinity);for(;k<elements.length&&clock['now']()<endTime;k++){var cs=elements[k];var attrs=EMPTY;{for(var preceder=cs;(preceder=preceder.previousSibling);){var nt=preceder.nodeType;var value=(nt===7||nt===8)&&preceder.nodeValue;if(value?!/^\??prettify\b/.test(value):(nt!==3||/\S/.test(preceder.nodeValue))){break;}
if(value){attrs={};value.replace(/\b(\w+)=([\w:.%+-]+)/g,function(_,name,value){attrs[name]=value;});break;}}}
var className=cs.className;if((attrs!==EMPTY||prettyPrintRe.test(className))&&!prettyPrintedRe.test(className)){var nested=false;for(var p=cs.parentNode;p;p=p.parentNode){var tn=p.tagName;if(preCodeXmpRe.test(tn)&&p.className&&prettyPrintRe.test(p.className)){nested=true;break;}}
if(!nested){cs.className+=' prettyprinted';var langExtension=attrs['lang'];if(!langExtension){langExtension=className.match(langExtensionRe);var wrapper;if(!langExtension&&(wrapper=childContentWrapper(cs))&&codeRe.test(wrapper.tagName)){langExtension=wrapper.className.match(langExtensionRe);}
if(langExtension){langExtension=langExtension[1];}}
var preformatted;if(preformattedTagNameRe.test(cs.tagName)){preformatted=1;}else{var currentStyle=cs['currentStyle'];var defaultView=doc.defaultView;var whitespace=(currentStyle?currentStyle['whiteSpace']:(defaultView&&defaultView.getComputedStyle)?defaultView.getComputedStyle(cs,null).getPropertyValue('white-space'):0);preformatted=whitespace&&'pre'===whitespace.substring(0,3);}
var lineNums=attrs['linenums'];if(!(lineNums=lineNums==='true'||+lineNums)){lineNums=className.match(/\blinenums\b(?::(\d+))?/);lineNums=lineNums?lineNums[1]&&lineNums[1].length?+lineNums[1]:true:false;}
if(lineNums){numberLines(cs,lineNums,preformatted);}
var prettyPrintingJob={langExtension:langExtension,sourceNode:cs,numberLines:lineNums,pre:preformatted,sourceCode:null,basePos:null,spans:null,decorations:null};applyDecorator(prettyPrintingJob);}}}
if(k<elements.length){win.setTimeout(doWork,250);}else if('function'===typeof opt_whenDone){opt_whenDone();}}
doWork();}
var PR=win['PR']={'createSimpleLexer':createSimpleLexer,'registerLangHandler':registerLangHandler,'sourceDecorator':sourceDecorator,'PR_ATTRIB_NAME':PR_ATTRIB_NAME,'PR_ATTRIB_VALUE':PR_ATTRIB_VALUE,'PR_COMMENT':PR_COMMENT,'PR_DECLARATION':PR_DECLARATION,'PR_KEYWORD':PR_KEYWORD,'PR_LITERAL':PR_LITERAL,'PR_NOCODE':PR_NOCODE,'PR_PLAIN':PR_PLAIN,'PR_PUNCTUATION':PR_PUNCTUATION,'PR_SOURCE':PR_SOURCE,'PR_STRING':PR_STRING,'PR_TAG':PR_TAG,'PR_TYPE':PR_TYPE,'prettyPrintOne':IN_GLOBAL_SCOPE?(win['prettyPrintOne']=$prettyPrintOne):(prettyPrintOne=$prettyPrintOne),'prettyPrint':IN_GLOBAL_SCOPE?(win['prettyPrint']=$prettyPrint):(prettyPrint=$prettyPrint)};var define=win['define'];if(typeof define==="function"&&define['amd']){define("google-code-prettify",[],function(){return PR;});}})();