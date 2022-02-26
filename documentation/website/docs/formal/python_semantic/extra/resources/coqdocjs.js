/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var coqdocjs = coqdocjs || {};
(function(){

function replace(s){
  var m;
  if (m = s.match(/^(.+)'/)) {
    return replace(m[1])+"'";
  } else if (m = s.match(/^([A-Za-z]+)_?(\d+)$/)) {
    return replace(m[1])+m[2].replace(/\d/g,
      function(d){return coqdocjs.subscr[d]});
  } else if (coqdocjs.repl.hasOwnProperty(s)){
    return coqdocjs.repl[s]
  } else {
    return s;
  }
}

function toArray(nl){
    return Array.prototype.slice.call(nl);
}

function replInTextNodes() {
  coqdocjs.replInText.forEach(function(toReplace){
    toArray(document.getElementsByClassName("code"))
      .concat(toArray(document.getElementsByClassName("inlinecode")))
      .forEach(function(elem){
      toArray(elem.childNodes).forEach(function(node){
        if (node.nodeType != Node.TEXT_NODE) return;
        var fragments = node.textContent.split(toReplace);
        node.textContent = fragments[fragments.length-1];
        for (var k = 0; k < fragments.length - 1; ++k) {
          node.parentNode.insertBefore(document
            .createTextNode(fragments[k]),node);
          var replacement = document.createElement("span");
          replacement.appendChild(document.createTextNode(toReplace));
          replacement.setAttribute("class", "id");
          replacement.setAttribute("type", "keyword");
          node.parentNode.insertBefore(replacement, node);
        }
      });
    });
  });
}

function replNodes() {
  toArray(document.getElementsByClassName("id")).forEach(function(node){
    if (["var", "variable", "keyword", "notation", "definition", "inductive"]
      .indexOf(node.getAttribute("type"))>=0){
      var text = node.textContent;
      var replText = replace(text);
      if(text != replText) {
        node.setAttribute("repl", replText);
        node.setAttribute("title", text);
        var hidden = document.createElement("span");
        hidden.setAttribute("class", "hidden");
        while (node.firstChild) {
          hidden.appendChild(node.firstChild);
        }
        node.appendChild(hidden);
      }
    }
  });
}

function isVernacStart(l, t){
  t = t.trim();
  for(var s of l){
    if (t == s || t.startsWith(s+" ") || t.startsWith(s+".")){
      return true;
    }
  }
  return false;
}

function isProofStart(s){
  return isVernacStart(["Proof"], s);
}

function isProofEnd(s){
  return isVernacStart(["Qed", "Admitted", "Defined", "Abort"], s);
}

function proofStatus(){
  var proofs = toArray(document.getElementsByClassName("proof"));
  if(proofs.length) {
    for(var proof of proofs) {
      if (proof.getAttribute("show") === "false") {
          return "some-hidden";
      }
    }
    return "all-shown";
  }
  else {
    return "no-proofs";
  }
}

function updateView(){
  document.getElementById("toggle-proofs").setAttribute("proof-status",
    proofStatus());
}

function foldProofs() {
  var hasCommands = true;
  var nodes = document.getElementsByClassName("command");
  if(nodes.length == 0) {
    hasCommands = false;
    console.log("no command tags found")
    nodes = document.getElementsByClassName("id");
  }
  toArray(nodes).forEach(function(node){
    if(isProofStart(node.textContent)) {
      var proof = document.createElement("span");
      proof.setAttribute("class", "proof");

      node.parentNode.insertBefore(proof, node);
      if(proof.previousSibling.nodeType === Node.TEXT_NODE)
        proof.appendChild(proof.previousSibling);
      while(node && !isProofEnd(node.textContent)) {
        proof.appendChild(node);
        node = proof.nextSibling;
      }
      if (proof.nextSibling) proof.appendChild(proof.nextSibling); // the Qed
      if (!hasCommands && proof.nextSibling)
        proof.appendChild(proof.nextSibling); // the dot after the Qed

      proof.addEventListener("click", function(proof){return function(e){
        if (e.target.parentNode.tagName.toLowerCase() === "a")
          return;
        proof.setAttribute("show",
          proof.getAttribute("show") === "true" ? "false" : "true");
        proof.setAttribute("animate", "");
        updateView();
      };}(proof));
      proof.setAttribute("show", "false");
    }
  });
}

function toggleProofs(){
  var someProofsHidden = proofStatus() === "some-hidden";
  toArray(document.getElementsByClassName("proof")).forEach(function(proof){
    proof.setAttribute("show", someProofsHidden);
    proof.setAttribute("animate", "");
  });
  updateView();
}

function repairDom(){
  // pull whitespace out of command
  toArray(document.getElementsByClassName("command")).forEach(function(node){
    while(node.firstChild && node.firstChild.textContent.trim() == ""){
      console.log("try move");
      node.parentNode.insertBefore(node.firstChild, node);
    }
  });
  toArray(document.getElementsByClassName("id")).forEach(function(node){
    node.setAttribute("type", node.getAttribute("title"));
  });
  toArray(document.getElementsByClassName("idref")).forEach(function(ref){
    toArray(ref.childNodes).forEach(function(child){
      if (["var", "variable"].indexOf(child.getAttribute("type")) > -1)
        ref.removeAttribute("href");
    });
  });

}

function fixTitle(){
  var url = "/" + window.location.pathname;
  var modulename = "." + url.substring(url.lastIndexOf('/')+1,
    url.lastIndexOf('.'));
  modulename = modulename.substring(modulename.lastIndexOf('.')+1);
  if (modulename === "toc") {modulename = "Table of Contents";}
  else if (modulename === "indexpage") {modulename = "Index";}
  else {modulename = modulename + ".v";};
  document.title = modulename;
}

function postprocess(){
  repairDom();
  replInTextNodes()
  replNodes();
  foldProofs();
  document.getElementById("toggle-proofs").addEventListener("click",
    toggleProofs);
  updateView();
}

fixTitle();
document.addEventListener('DOMContentLoaded', postprocess);

coqdocjs.toggleProofs = toggleProofs;
})();
