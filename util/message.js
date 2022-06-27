var TRS_changed  = false;
var Calc_changed = false;
var N = 1;
var ruleCount = 0;
var signMathFieldSpan = document.getElementById(`rules-sign-math-field`);
var signMathFieldSpan = MQ.MathField(signMathFieldSpan, {spaceBehavesLikeTab: true});
addRuleButton("");

// window.addEventListener('beforeunload', function (e) {
//     e.preventDefault();
//     e.returnValue = 'Do you want to leave the page?';
//     setTimeout(function () { // Timeout to wait for user response
//         setTimeout(function () { // Timeout to wait onunload, if not fired then this will be executed
//             console.log('User stayed on the page.');
//     }, 50)}, 50);
//     return 'Do you want to leave the page?';
// });

initCalc();

function initCalc() {
  var mathFieldSpan = document.getElementById('calc-math-field');
  var mathField = MQ.MathField(mathFieldSpan, {
    spaceBehavesLikeTab: true,
    handlers: {
      edit: function() {
        Calc_changed = true;
        var resLatex = document.getElementById("calc-result-math-field");
        resLatex.textContent="";
      },
      enter: function() {
        if (Calc_changed) {
          calc();
        }
      },
    }
  });
  mathField.focus();

  document.getElementById("add-omega-calc").onmousedown = function () {
    event.preventDefault();
    var elements = document.getElementsByClassName("mq-focused");
    if (elements[0].classList.contains("mq-calc")) {
      var editable = MQ(elements[0]);
      editable.cmd("\\omega");
    }
  };
}

function calc() {
  var calc =  MQ(document.getElementById("calc-math-field"));
  var latexCalc = calc.latex();
  latexCalc = encodeURIComponent(latexCalc);
  var xhr = new XMLHttpRequest();
  xhr.open("POST", 'http://localhost:8002/calc?expr=\"' + latexCalc + '!\"', true);

  xhr.onload = function() {
    var resp = JSON.parse(this.responseText);
    if (resp['errors'] != '') {
      alert(resp.errors);
    } else {
      var res = resp['result'];
      var resLatex = document.getElementById("calc-result-math-field");
      resLatex.textContent=res;
      var calcField = MQ.StaticMath(resLatex);
      calcField.reflow();
    }
  };

  xhr.send();
}

function createRule(elemId, rule) {
  var container = document.createElement('div');
  container.innerHTML = `<li style="font-size:20px" id = "rule-${elemId}">
    <input class="formula-delete" type="button" value="X"/> Rule: <span style="overflow:auto;width:60%;font-size:16px" class="mq-rule" id="rule-math-field-${elemId}">${rule}</span>
    </li>`
  return container.firstChild
}

function createFormula(elemId, arity) {
  var container = document.createElement('div');
  var param = arity === 0 ? '' : `( <span style="overflow:auto;width:20%;font-size:16px" class="mq-args" id="formula-sign-math-field-${elemId}"></span> )`
  container.innerHTML = `<li style="font-size:20px" id = "func-${elemId}">
    ${elemId}${param} = <span style="overflow:auto;width:60%;font-size:16px" class="mq-func" id="formula-math-field-${elemId}"></span>
  </li>`

  return container.firstChild
}

function createFuncHeader() {
  var td_funcs = document.getElementById('td_functions');
  td_funcs.innerHTML = `<div><input type="button" value="Submit Functions" onclick="checkTerminationButton()"></div>
    <button id="add-omega">\u03C9</button>
    <p>N: <span id="counter">1</span></p>
    <button onclick="incrementCounter()">+</button>
    <button onclick="decrementCounter()">-</button>
    <ol id="formulas" style="list-style: none;">
    </ol>`
  return
}

function incrementCounter() {
  const counterElem = document.getElementById('counter');
  count = Number(counterElem.innerHTML);
  counterElem.innerHTML = count+1;
  N = N + 1;
}

function decrementCounter() {
  if (N > 1) {
    const counterElem = document.getElementById('counter');
    count = Number(counterElem.innerHTML);
    counterElem.innerHTML = count-1;
    N = N - 1;
  }
}

function addDeleteOnClick(ruleElem) {
  var input = ruleElem.getElementsByTagName('INPUT')[0];
  input.onclick = function() {
    var l = document.getElementById('rules').children.length;
    if (l > 1) {
      ruleElem.parentNode.removeChild(ruleElem);
    }
  }
}

function insertExample(exId) {
  var example = document.getElementById('examples').getElementsByTagName('li')[exId];
  var rulesEx = example.getElementsByClassName("mq-ex");
  var params = example.getElementsByTagName('param')[0].value;
  document.getElementById("rules-sign-math-field").textContent = params;
  MQ.MathField(document.getElementById("rules-sign-math-field"), {spaceBehavesLikeTab: true});

  var rules = document.getElementById('rules');
  rules.innerHTML = '';
  for (let i = 0; i < rulesEx.length; i++) {
    addRuleButton(rulesEx[i].textContent);
  }
  formulaInputButton();
}

function addRuleButton(ruleText) {
  var rules = document.getElementById('rules');
  var elemId = ruleCount;
  ruleCount += 1;
  var ruleElem = createRule(elemId, ruleText);

  rules.appendChild(ruleElem);
  addDeleteOnClick(ruleElem);

  var mathFieldSpan = document.getElementById(`rule-math-field-${elemId}`);
  var mathField = MQ.MathField(mathFieldSpan, {
    spaceBehavesLikeTab: true,
    handlers: {
      edit: function() {
        TRS_changed = true;
      },
      enter: function() {
        addRuleButton("");
      },
      upOutOf: function(mathField) {
        var rules = Array.prototype.slice.call(document.getElementById('rules').children);
        var liRef = document.getElementById(`rule-${elemId}`);
        var index = rules.indexOf(liRef);
        var id = (index > 0) ? index - 1 : rules.length - 1;
        var upField = MQ((rules[id].getElementsByClassName("mq-rule"))[0]);
        upField.focus();
      },
      downOutOf: function(mathField) {
        var rules = Array.prototype.slice.call(document.getElementById('rules').children);
        var liRef = document.getElementById(`rule-${elemId}`);
        var index = rules.indexOf(liRef);
        var id = (index < rules.length - 1) ? index + 1 : 0;
        var upField = MQ((rules[id].getElementsByClassName("mq-rule"))[0]);
        upField.focus();
      }
    }
  });

  mathField.focus();
}

function addMQ(elemId) {
    var mathFieldSpan = document.getElementById(`formula-math-field-${elemId}`);
    var mathField = MQ.MathField(mathFieldSpan, {
      spaceBehavesLikeTab: true,
      handlers: {
        upOutOf: function(mathField) {
          var rules = Array.prototype.slice.call(document.getElementById('formulas').children);
          var liRef = document.getElementById(`func-${elemId}`);
          var index = rules.indexOf(liRef);
          var id = (index > 0) ? index - 1 : rules.length - 1;
          var upField = MQ((rules[id].getElementsByClassName("mq-func"))[0]);
          upField.focus();
        },
        downOutOf: function(mathField) {
          var rules = Array.prototype.slice.call(document.getElementById('formulas').children);
          var liRef = document.getElementById(`func-${elemId}`);
          var index = rules.indexOf(liRef);
          var id = (index < rules.length - 1) ? index + 1 : 0;
          var upField = MQ((rules[id].getElementsByClassName("mq-func"))[0]);
          upField.focus();
        },
        moveOutOf: function(direction) {
          if (direction == MQ.L) {
            var rules = Array.prototype.slice.call(document.getElementById('formulas').children);
            var liRef = document.getElementById(`func-${elemId}`);
            var index = rules.indexOf(liRef);
            var upField = MQ((rules[index].getElementsByClassName("mq-args"))[0]);
            if (upField){
              upField.focus();
            }
          }
        }
      }
    });
    var signMathFieldSpan = document.getElementById(`formula-sign-math-field-${elemId}`);
    var signMathFieldSpan = MQ.MathField(signMathFieldSpan, {
      spaceBehavesLikeTab: true,
      handlers: {
        moveOutOf: function(direction) {
          if (direction == MQ.R) {
            var rules = Array.prototype.slice.call(document.getElementById('formulas').children);
            var liRef = document.getElementById(`func-${elemId}`);
            var index = rules.indexOf(liRef);
            var upField = MQ((rules[index].getElementsByClassName("mq-func"))[0]);
            upField.focus();
          }
        }
      }
    });
}

function formulaInputButton() {
  var rules = document.getElementById('rules').getElementsByTagName('li');
  var rules_lenght = rules.length;

  var params =  MQ((document.getElementsByClassName("mq-params"))[0]);
  var latexParams = params.latex();
  if (/[^a-zA-Z,\\]/i.test(latexParams)) {
    alert(`parameters has forbidden symbols`);
    return;
  }
  var rules_latex = '\\left\\{' + latexParams + '\\right\\}';

  if (rules_lenght > 0) {
    var inc_rules = [];
    for (let i = 0; i < rules_lenght; i++) {
      var rule = MQ((rules[i].getElementsByClassName("mq-rule"))[0]);
      var latexRule = rule.latex();
      var latexParams = params.latex();
      if (/[^a-zA-Z,\(\)\\\=]/i.test(latexRule)) {
      	inc_rules.push(i+1);
      } else {
        rules_latex = rules_latex + latexRule + ';';
      }
    }

    if (inc_rules > 0) {
      alert(`rules: ${inc_rules}\nhas forbidden symbols`);
    } else {
      TRS_changed = false;
      rules_latex = encodeURIComponent(rules_latex);
      var xhr = new XMLHttpRequest();
      xhr.open("POST", 'http://localhost:8002/trs?rules=\"' + rules_latex + '!\"', true);

      xhr.onload = function() {
        var resp = JSON.parse(this.responseText);
        if (resp['errors'] != '') {
          alert(resp.errors);
        } else {
          var funcs = resp.funcs;

          createFuncHeader();

          var formulas = document.getElementById('formulas');
          formulas.innerHTML = '';

          document.getElementById("add-omega").onmousedown = function () {
            event.preventDefault();
            var elements = document.getElementsByClassName("mq-focused");
            if (elements[0].classList.contains("mq-func")) {
              var editable = MQ(elements[0]);
              editable.cmd("\\omega");
            }
          };

          for (var prop in funcs) {
            if (Object.prototype.hasOwnProperty.call(funcs, prop)) {
              console.log(prop + ':' + funcs[prop]);
              var formulaElem = createFormula(prop, funcs[prop]);
              formulas.appendChild(formulaElem);
              addMQ(prop);
            }
          }
        }
      };

      xhr.send();
    }
  }
}


function checkTerminationButton() {
  if (TRS_changed) {
    alert("TRS has been edited, please submit current version");
    return;
  }

  var params =  MQ((document.getElementsByClassName("mq-params"))[0]);
  var latexParams = params.latex();
  if (/[^a-zA-Z,\\]/i.test(latexParams)) {
    alert(`parametrs has forbidden symbols`);
    return;
  }
  var rules_latex = '\\left\\{' + latexParams + '\\right\\}';

  var rules = document.getElementById('rules').getElementsByTagName('li');
  var rules_lenght = rules.length;

  if (rules_lenght > 0) {

    for (let i = 0; i < rules_lenght; i++) {
      var rule = MQ((rules[i].getElementsByClassName("mq-rule"))[0]);
      var latex = rule.latex();
      rules_latex = rules_latex + latex + ';';
    }

    var inc_funcs = [];
    var funcs_latex = '';

    var funcs = document.getElementById('formulas').getElementsByTagName('li');
    for (let i = 0; i < funcs.length; i++) {
      var func = MQ(funcs[i].getElementsByClassName("mq-func")[0]);
      var f_name = funcs[i].getElementsByClassName("mq-func")[0].id.substr(19);
      var latex_func = func.latex();
      var sign = MQ(funcs[i].getElementsByClassName("mq-args")[0]);
      var latex_sign = (sign != null) ? sign.latex() : '';
      if (latex_func === '') {
        alert(`Please, declare function ${f_name}`);
        return;
      }
      if (/[\^\{\}\(\)\\x0-9 ]|(\\omega)|(\\cdot)/.test(latex) || /[^a-zA-Z,\\]/i.test(latex_sign)) {
        if (N == 1) {
          if (latex_sign != '') {
            latex_sign_elems = latex_sign.split(',');
            latex_sign = '\\left(' + latex_sign_elems[0] + '\\right)';
            for (let j = 1; j < latex_sign_elems.length; j++) {
              latex_sign = latex_sign + ',\\left(' + latex_sign_elems[j] + '\\right)'
            }
          }
          latex_func = '\\left(' + latex_func + '\\right)'
        }
        funcs_latex = funcs_latex + f_name + '\\left(' + latex_sign + '\\right)' + '=' + latex_func + ';';
      } else {
        inc_funcs.push(f_name);
      }
    }

    if (inc_funcs.length > 0) {
      alert(`Functions: ${inc_funcs}\n : forbidden symbols in the definitions`);
    } else {
      rules_latex = 'rules=\"' + encodeURIComponent(rules_latex) + '!\"';
      funcs_latex = 'funcs=\"' + encodeURIComponent(funcs_latex) + '!\"';
      var xhr = new XMLHttpRequest();
      xhr.open("POST", 'http://localhost:8002/prove?' + rules_latex + '&' + funcs_latex + '&' + `n=${N}`, true);

      xhr.onload = function() {
        var resp = JSON.parse(this.responseText);
        if (resp['errorsF'] != '') {
          alert('Syntax errors in the following functions:\n' + resp.errorsF);
        } else if (resp['errorsR'] != '') {
          alert('The following rules does not satisfy the termination criterion:\n' + resp.errorsR);
        } else {
          alert('The given interpretation proves termination of the TRS!');
        }
      };

      xhr.send();
    }
  }
}
