var TRS_changed = false;

function insertSqrt() {
    var MQ = MathQuill.getInterface(2);
    var elements = document.getElementsByClassName("mq-focused");
    var editable = MQ(elements[0]);
    editable.write("\\sqrt");
}

function createRule(elemId) {
  var container = document.createElement('div');

  container.innerHTML = `<li><div>
    <p>Rule: <span id="rule-math-field-${elemId}"></span></p>
    <p>LaTeX: <span id="rule-latex-${elemId}"></span></p>
    <input class="formula-delete" type="button" value="remove"/>
  </div></li>`

  return container.firstChild
}

function addDeleteOnClick(ruleElem) {
  var input = ruleElem.getElementsByTagName('INPUT')[0];
  input.onclick = function() {
    ruleElem.parentNode.removeChild(ruleElem)
  }
}

function addRuleButton() {
  var rules = document.getElementById('rules');
  var elemId = rules.getElementsByTagName('li').length;
  var ruleElem = createRule(elemId);

  rules.appendChild(ruleElem);
  addDeleteOnClick(ruleElem);

  var mathFieldSpan = document.getElementById(`rule-math-field-${elemId}`);
  var latexSpan = document.getElementById(`rule-latex-${elemId}`);

  var mathField = MQ.MathField(mathFieldSpan, {
    spaceBehavesLikeTab: true,
    handlers: {
      edit: function() {
        latexSpan.textContent = mathField.latex();
        TRS_changed = true;
      }
    }
  });
}

function createFormula(elemId, arity) {
  var container = document.createElement('div');
  var param = arity === 1 ? '(x)' : ''
  container.innerHTML = `<li><div>
    <p>${elemId}${param} = <span class="mq-func" id="formula-math-field-${elemId}"></span></p>
    <p>LaTeX: <span id="formula-latex-${elemId}"></span></p>
  </div></li>`

  return container.firstChild
}

function createFuncHeader() {
  var td_funcs = document.getElementById('td_functions');
  td_funcs.innerHTML = `<h3>Proving Functions:</h3>
    <div><input type="button" value="Submit Functions" onclick="checkTerminationButton()"></div>
    <button id="add-omega">\u03C9</button>`
  return
}

function addMQ(id) {
    var mathFieldSpan = document.getElementById(`formula-math-field-${id}`);
    var latexSpan = document.getElementById(`formula-latex-${id}`);

    var mathField = MQ.MathField(mathFieldSpan, {
      spaceBehavesLikeTab: true,
      handlers: {
        edit: function() {
          latexSpan.textContent = mathField.latex();
        }
      }
    });
}

function formulaInputButton() {
  var rules = document.getElementById('rules');
  var rules_lenght = rules.getElementsByTagName('li').length;

  var formulas = document.getElementById('formulas');
  formulas.innerHTML = '';

  if (rules_lenght > 0) {
    var inc_rules = [];
    var rules_latex = 'rules=\"';

    for (let i = 0; i < rules_lenght; i++) {
      var rule = MQ(document.getElementById(`rule-math-field-${i}`));
      var latex = rule.latex();
      if (/[^a-zA-Z\(\)\\\=]/i.test(latex) || latex.includes('\\omega')) {
      	inc_rules.push(i+1);
      } else {
        rules_latex = rules_latex + encodeURIComponent(latex + ';');
      }
    }

    if (inc_rules > 0) {
      alert(`rules: ${inc_rules}\nhas forbidden symbols`);
    } else {
      TRS_changed = false;
      var xhr = new XMLHttpRequest();
      xhr.open("POST", 'http://localhost:8001/trs?' + rules_latex + '!\"', true);

      xhr.onload = function() {
        var resp = JSON.parse(this.responseText);
        if (resp['errors'] != '') {
          alert(resp.errors);
        } else {
          var funcs = resp.funcs;

          createFuncHeader();


          document.getElementById("add-omega").onmousedown = function () {
            event.preventDefault();
            var elements = document.getElementsByClassName("mq-focused");
            var editable = MQ(elements[0]);
            editable.cmd("\\omega");
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

  var rules = document.getElementById('rules');
  var rules_lenght = rules.getElementsByTagName('li').length;

  if (rules_lenght > 0) {
    var rules_latex = 'rules=\"';

    for (let i = 0; i < rules_lenght; i++) {
      var rule = MQ(document.getElementById(`rule-math-field-${i}`));
      var latex = rule.latex();
      rules_latex = rules_latex + encodeURIComponent(latex + ';');
    }

    var inc_funcs = [];
    var funcs_latex = 'funcs=\"';

    var funcs = document.getElementsByClassName("mq-func");
    for (let i = 0; i < funcs.length; i++) {
      var func = MQ(funcs[i]);
      var f_name = funcs[i].id.substr(19);
      var latex = func.latex();
      if (latex === '') {
        alert(`Please, declare function ${f_name}`);
        return;
      }
      if (/[\^\{\}\(\)\\x0-9 ]|(\\omega)|(\\cdot)/.test(latex)) {
        funcs_latex = funcs_latex + encodeURIComponent(f_name + '=' + latex + ';');
      } else {
        inc_funcs.push(f_name);
      }
    }

    if (inc_funcs.length > 0) {
      alert(`Functions: ${inc_funcs}\nhas forbidden symbols`);
    } else {
      rules_latex = rules_latex + '!\"';
      funcs_latex = funcs_latex + '!\"';
      var xhr = new XMLHttpRequest();
      xhr.open("POST", 'http://localhost:8001/prove?' + rules_latex + '&' + funcs_latex, true);

      xhr.onload = function() {
        var resp = JSON.parse(this.responseText);
        console.log(resp);
        if (resp['errorsF'] != '') {
          alert('Got errors in this functions\' declarations:\n' + resp.errorsF);
        } else if (resp['errorsR'] != '') {
          alert('Next rules does not satisfy termination criteria:\n' + resp.errorsR);
        } else {
          alert('TRS terminate!');
        }
      };

      xhr.send();
    }
  }
}
