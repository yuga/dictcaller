document.addEventListener('DOMContentLoaded', function () {
    restoreOptions();
    document.getElementById('saveOptions').addEventListener('click', saveOptions);
    document.getElementById('shortcutEnable').addEventListener('click', updateShortcutFields);
});	

Storage.prototype.setObject = function (key, value) {    
    this.setItem(key, JSON.stringify(value));
}

Storage.prototype.getObject = function (key) {
    var value = this.getItem(key);
    if (value == null) {
        return null;
    } else {
        return JSON.parse(value);
    }
}

// Set the active option in the <select> named select_name to choice.
function setSelection(select_name, choice) {
    var select = document.getElementById(select_name);
    for (var i in select.children) {
        var child = select.children[i];
        if (child.value == choice) {
            child.selected = 'true';
            break;
        }
    }
}

var selects    = ['lookupModifier', 'lookupKey', 'shortcutModifier', 'shortcutKey'];
var checkboxes = ['shortcutEnable', 'hideWithEscape', 'saveFrameSize'];
var textboxes  = ['searchUrl'];
var numboxes   = ['frameWidth', 'frameHeight', 'queryFormWidth', 'inframeMargin'];

// Restores state from localStorage.
function restoreOptions() {
    // Set defaults.
    document.getElementById('searchUrl').value = "http://localhost:8000/search";
    setSelection('lookupModifier', 'Ctrl');
    setSelection('lookupKey', 'Q');
    setSelection('shortcutModifier', 'Alt');
    setSelection('shortcutKey', 'Q');
    document.getElementById('shortcutEnable').checked = true;
    document.getElementById('frameWidth').value = 550;
    document.getElementById('frameHeight').value = 250;
    document.getElementById('queryFormWidth').value = 250;
    document.getElementById('hideWithEscape').checked = true;
    document.getElementById('saveFrameSize').checked = true;
    document.getElementById('inframeMargin').value = 40

    // Override defaults by saved settings.
    for (var i in selects) {
        var select = selects[i];
        var choice = localStorage.getObject(select);
        if (choice != null) setSelection(select, choice);
    }

    for (var i in checkboxes) {
        var checkbox = checkboxes[i];
        var checked = localStorage.getObject(checkbox);
        if (checked != null) document.getElementById(checkbox).checked = checked;
    }

    for (var i in textboxes) {
        var textbox = textboxes[i];
        var val = localStorage.getObject(textbox);
        if (val != null) document.getElementById(textbox).value = val;
    }

    for (var i in numboxes) {
        var textbox = numboxes[i];
        var val = localStorage.getObject(textbox);
        if (val != null) document.getElementById(textbox).value = Math.round(val);
    }

    updateShortcutFields();
}

// Saves state to localStorage.
function saveOptions() {
    for (var i in selects) {
        var select = selects[i];
        localStorage.setObject(select, document.getElementById(select).value);
    }

    for (var i in checkboxes) {
        var checkbox = checkboxes[i];
        localStorage.setObject(checkbox, document.getElementById(checkbox).checked);
    }

    for (var i in textboxes) {
        var textbox = textboxes[i];
        localStorage.setObject(textbox, document.getElementById(textbox).value);
    }

    for (var i in numboxes) {
        var textbox = numboxes[i];
        var value = parseInt(document.getElementById(textbox).value);
        if (!isNaN(value)) localStorage.setObject(textbox, value);
    }

    // Fade in status message.
    var status = document.getElementById('saveStatusMessage');
    status.style.opacity = 1;
    setTimeout(function () {
        status.style.opacity = 0;
    }, 1500);
}

function updateShortcutFields() {
    checked = document.getElementById('shortcutEnable').checked;
    document.getElementById('shortcutModifier').disabled = !checked;
    document.getElementById('shortcutKey').disabled = !checked;
}
