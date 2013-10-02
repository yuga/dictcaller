
// CSS-related constants. Should be synced with frame.css.
var ROOT_ID        = 'chrome_dictcaller_ext';
var FORM_ID        = ROOT_ID + '_form';
var PADDING_TOP    = 8;
var PADDING_RIGHT  = 5;
var PADDING_BOTTOM = 15;
var PADDING_LEFT   = 10;
var PADDING_FORM   = 10;
var BASE_Z_INDEX   = 65000;
var POPUP_SPACEY   = 15;

// Path/URL Constants.
var HANDLE_ICON_URL = chrome.extension.getURL('handle.png');

// Internal global vars.
var body = document.getElementsByTagName('body')[0];

// Extension options with defaults.
var options = {
  searchUrl:         'http://localhost:8000/search',
  sayUrl:            'http://localhost:8000/say',
  lookupModifier:    'Ctrl',
  lookupKey:         'Q',
  sayWordKey:        'W',
  sayParagraphKey:   'P',
  shortcutModifier:  'Alt',
  shortcutKey:       'Q',
  shortcutEnable:    true,
  frameWidth:        550,
  frameHeight:       250,
  queryFormWidth:    250,
  queryFormHeight:   50,  // This one is an approximation for centering.
  hideWithEscape:    true,
  saveFrameSize:     true,
  inframeMargin:     40
};

// Query blocker
var queryBlock = {
  prevQuery: '',
  timeoutId: 0,
  enter: function(query) {
    var result = false;
    if (this.prevQuery != query) {
      result = true;
    }
    if (this.timeoutId != 0) {
      clearTimeout(this.timeoutId);
    }
    this.timeoutId = setTimeout(leavequeryBlock, 2000);
    this.prevQuery = query;
    return result;
  },
  leave: function() {
    this.timeoutId = 0;
    this.prevQuery = "";
  }
}

var leavequeryBlock = function() {
  queryBlock.leave();
}

// latest mouse event information
var mouseInfo = {
  x: 0,
  y: 0,
  pageX: 0,
  pageY: 0,
  clientX: 0,
  clientY: 0,
  setMouseEvent: function(e) {
    this.x = e.x;
    this.y = e.y;
    this.pageX = e.pageX;
    this.pageY = e.pageY;
    this.clientX = e.clientX;
    this.clientY = e.clientY;
  }
}

/***************************************************************
 *                          Entry Point                        *
 ***************************************************************/
// Main initialization function. Loads options and sets listeners.
var initialize = function() {
  // Run in top frame only.
  //if (window != top) return;

  // Load options.
  var setOpt = function (opt) {
    chrome.extension.sendMessage({method: 'retrieve', arg: opt}, function(response) {
      if (response) options[opt] = response;
    });
  }
  
  for (var opt in options) {
    setOpt(opt);
  }
  
  // Manually inject the stylesheet into non-HTML pages that have no <head>.
  if (!document.head) {
    link = document.createElement('link');
    link.rel = 'stylesheet';
    link.type = 'text/css';
    link.href = chrome.extension.getURL('frame.css');
    document.body.appendChild(link);
  }
  
  // Set event listeners.
  window.addEventListener('keydown', handleKeypress, false);
  setTimeout(function() {
    window.addEventListener('mousemove', handleMouseOver, false);
  }, 100);
}

/***************************************************************
 *                        Event Handlers                       *
 ***************************************************************/
// Handle lookup-on-mouse-over.
var  handleMouseOver = function (e) {
  // store mouse position
  mouseInfo.setMouseEvent(e);

  // Ignore clicks inside frame.
  is_inside_frame = isClickInsideFrame(e);

  // Remove frame or form if one is displayed.
  if (!is_inside_frame) {
    if (!document.getElementById(FORM_ID)) {
      removePopup(true, true);
    }
  }
}

// Handle keyboard shortcut.
var handleKeypress = function (e) {
  if (options.hideWithEscape && e.keyCode == 27) {
    removePopup(true, true);
    return;
  }

  if (checkModifier(options.lookupModifier, e)) {
    if (options.lookupKey.charCodeAt(0) == e.keyCode) {
      var queries = getQueries(mouseInfo.x, mouseInfo.y);
      if (queries.length > 0) {
        executeQuery(queries, mouseInfo.pageX, mouseInfo.pageY,
                     mouseInfo.clientX, mouseInfo.clientY, false);
        e.preventDefault();
      }
    } else if (options.sayWordKey.charCodeAt(0) == e.keyCode) {
      var word = getSaidWord(mouseInfo.x, mouseInfo.y);
      if (word.length > 0) {
        // executeSay(word);
        e.preventDefault();
      }
    } else if (options.sayParagraphKey.charCodeAt(0) == e.keyCode) {
      var paragraph = getSaidParagraph(mouseInfo.x, mouseInfo.y);
      if (paragraph.length > 0) {
        // executeSay(paragraph);
        e.preventDefault();
      }
    }
    return;
  } else if (!options.shortcutEnable
           || !checkModifier(options.shortcutModifier, e)
           || options.shortcutKey.charCodeAt(0) != e.keyCode) {
    return;
  }
  
  // Show query form if it's not already visible or clear it otherwise.
  if (!document.getElementById(FORM_ID)) {
    removePopup(true, false);
    grayOut(true);
    createQueryForm();
  } else {
    document.getElementById(FORM_ID).getElementsByTagName('input')[0].value = '';
  }
}

var getQueries = function (x, y) {
  var queries = []
  if (event.target.textContent) {
    var range = document.caretRangeFromPoint(x, y);
    if (expandRangeToWord(range)) {
      var query = range.toString();
      var prevquery = query;
      queries = expandQuery(query);
      var prevappended = queries;
      var count = 0;
      while(expandRangeToNextWord(range) && count < 5){
        ++count;
        query = range.toString();
        if (!query.match(/[^A-Za-z0-9\s]/)){
          var re = new RegExp(prevquery + '\\s*');
          var diff = query.replace(re, '');
          var expanded = expandQuery(diff);
          var appended = cartesianAppend(prevappended, expanded, true);
          queries = appended.concat(queries);
          prevappended = appended;
        }
        prevquery = query;
      }
    }
  }
  return queries;
}

var getSaidWord = function (x, y) {
  var word = "";
  if (event.target.textContent) {
    var range = document.caretRangeFromPoint(x, y);
    if (expandRangeToWord(range)) {
      word = range.toString();
    }
  }
  return word; 
}

var getSaidParagraph = function (x, y) {
  var paragraph = ""; 
  if (event.target.textContent) {
    var range = document.caretRangeFromPoint(x, y);
    if (expandRangeToWord(range)) {
      var count = 0;
      while (expandRangeToNextWord(range) && count < 5){
        ++count;
      }
      paragraph = range.toString();
      var re = new RegExp('\\s*$');
      var diff = paragraph.replace(re, '');
    }
  }
  return paragraph;
}

var expandRangeToWord = function (range) {
  var startOfWord = /^[^\w][\w]+$/;
  var endOfWord = /^[\w\-]+[^\w\-]$/;
  var whitespace = /^[\s.]+$/;
  var flag = 0;

  range.setStart(range.startContainer, range.startOffset - 1);
  while (whitespace.test(range.toString())) {
    range.setEnd(range.endContainer, range.endOffset + 1);
    range.setStart(range.startContainer, range.startOffset + 1);
  }

  while (!startOfWord.test(range.toString()) && flag == 0) {
    try{
      if (range.startOffset > 0) {
        range.setStart(range.startContainer, range.startOffset - 1);
      } else {
        flag = 1;
      }
    } catch(err) {
      return false;
    }
  }
  if (flag != 1) {
    range.setStart(range.startContainer, range.startOffset + 1);
  } else {
    flag = 0;
  }

  while (!endOfWord.test(range.toString()) && flag == 0) {
    try {
      if (range.endContainer.length > range.endOffset) {
        range.setEnd(range.endContainer, range.endOffset + 1);
      } else {
        flag = 1;
      }
    } catch(err) {
      return false;
    }
  }

  if (flag != 1) {
    range.setEnd(range.endContainer, range.endOffset - 1);
  }
  return true;
}

var expandRangeToNextWord = function (range) {
  var endOfWord = /[\w\-]+[^\w\-]$/;
  var whitespace = /[\s\n\r]$/;
  var flag = 0;
  
  try{
    range.setEnd(range.endContainer, range.endOffset + 1);
  } catch(err) {
    return false;
  }
  
  while (whitespace.test(range.toString())) {
    ++cnt;
    try {
      range.setEnd(range.endContainer, range.endOffset + 1);
    } catch(err) {
      flag=1;
      break;
    }
  }

  var cnt = 0;
  while (!endOfWord.test(range.toString())) {
    ++cnt;
    try {
      range.setEnd(range.endContainer, range.endOffset + 1);
    } catch(err) {
      flag = 1;
      break;
    }
  }
  if (cnt == 0) {
    return false;
  }
  if (flag != 1) {
    range.setEnd(range.endContainer, range.endOffset - 1);
  }
  return true;
}

var expandQuery = function (query) {
  var words = query.split('-');
  if (words.length < 2 || words.length > 3) {
    return expandWord(query);
  }
  var queries = [];
  var singles = [];
  for (var i = 0; i < words.length; ++i) {
    var expandedWords = expandWord(words[i]);
    singles = cartesianAppend(queries, expandedWords, true);
    queries = singles.concat(queries);
  }
  var results = []
  for (var i = 0; i < queries.length; ++i) {
    results.push(queries[i].split(' ').join('-'));
    results.push(queries[i]);
  }
  return results;
}

var expandWord = function (word) {
  var words = [ word ];

  if (!word.match(/[^A-Za-z0-9\s]/) ){
    if (word != word.toLowerCase()){
      words.push(word.toLowerCase());
    }

    if (word == 'has' || word == 'had') {
      words.push('have');
    }

    if (word == 'made') {
      words.push('make');
    }

    if (word.match(/ed$/)){
      words.push(word.replace(/d$/,''));
      words.push(word.replace(/ed$/,''));
    }

    if (word.match(/ied$/)){
      words.push(word.replace(/ied$/,'y'));
    }

    if (word.match(/[bdprt]{2}ed$/)){
      words.push(word.replace(/([bdprt]){2}ed$/,'\1'));
    }

    if (word.match(/ies$/)){
      words.push(word.replace(/ies$/,'y'));
    }

    if (word.match(/ier$/)){
      words.push(word.replace(/ier$/,'y'));
    }

    if (word.match(/er$/)){
      words.push(word.replace(/er$/,''));
    }
    if (word.match(/iest$/)){
      words.push(word.replace(/iest$/,'y'));
    }

    if (word.match(/est$/)){
      words.push(word.replace(/est$/,''));
    }

    if (word.match(/s$/)){
      words.push(word.replace(/s$/,''));
    }

    if (word.match(/nning$/)){
      words.push(word.replace(/nning$/,'n'));
    } else if (word.match(/ing$/)){
      words.push(word.replace(/ing$/,'e'));
      words.push(word.replace(/ing$/,''));
    }
  }
  return words;
}

var cartesianAppend = function (arrayX, arrayY, leftAxis) {
  var arrayZ = []
  var arrayA = leftAxis ? arrayX : arrayY;
  var arrayB = leftAxis ? arrayY : arrayX;

  for (var a in arrayA) {
    for (var b in arrayB) {
      var element = leftAxis
                  ? (arrayA[a] + ' ' + arrayB[b])
                  : (arrayB[b] + ' ' + arrayA[a]);
      arrayZ.push(element);
    }
  }
  return arrayZ;
}

var executeQuery = function (queries, pageX, pageY, clientX, clientY, fixed) {
  if (queryBlock.enter(queries[0])) {
    var url = options.searchUrl;
    var callback = function(json) {
      var results = JSON.parse(json);
      if (results) {
        createPopup(queries, results, pageX, pageY, clientX, clientY, fixed);
      }
    }
    var params = "q=" + escape(utf8encode(JSON.stringify(queries))) + "&m=json";
    chrome.extension.sendMessage({'action':'executeQuery', 'url':url, 'params':params}, callback);
  }
}

var say = function (text) {
  if (queryBlock.enter(text)) {
    var url = options.sayUrl;
    var params = "q=" + escape(utf8encode(JSON.stringify(queries))) + "&m=json";
    chrome.extension.sendMessagge({'action':'say', 'url':url, 'params':prams});
  }
}

/***************************************************************
 *                        UI Controllers                       *
 ***************************************************************/
// Creates and shows the manual query form.
var createQueryForm = function () {
  // Calculate the coordinates of the middle of the window.
  var windowX = (window.innerWidth - (PADDING_LEFT + options.queryFormWidth + PADDING_RIGHT)) / 2 ;
  var windowY = (window.innerHeight - (PADDING_TOP + options.queryFormHeight + PADDING_BOTTOM)) / 2;
  var x = body.scrollLeft + windowX;
  var y = body.scrollTop + windowY;

  // Create the form, set its id and insert it.
  var qform = document.createElement('div');
  qform.id = FORM_ID;
  body.appendChild(qform);

  // Set form style.
  var zoom_ratio = getZoomRatio();
  qform.style.position = 'absolute';
  qform.style.left = (x / zoom_ratio) + 'px';
  qform.style.top = (y / zoom_ratio) + 'px';
  qform.style.width = options.queryFormWidth + 'px';
  qform.style.zIndex = BASE_Z_INDEX;

  // Add textbox.
  textbox = document.createElement('input');
  textbox.type = 'text';
  qform.appendChild(textbox);

  var initLookup = function () {
    grayOut(false);
    removePopup(false, true);
    if (textbox.value.replace(/^\s+|\s+$/g, '') != '') {
      createCenteredPopup(textbox.value);
    }
  }

  textbox.focus();

  // Add button.
  button = document.createElement('input');
  button.type = 'button';
  button.value = 'Search';
  qform.appendChild(button);
  
  // Set lookup event handlers.
  textbox.addEventListener('keypress', function(e) {
    if (e.keyCode == 13) {  // Pressed Enter.
      setTimeout(initLookup, 400);
    }
  }, false);

  button.addEventListener('click', function(e) {
    setTimeout(initLookup, 400);
  }, false);

  // Schedule a resize of the textbox to accomodate the button in a single line.
  setTimeout(function() {
    var width = options.queryFormWidth - button.offsetWidth - 2 * PADDING_FORM - 3;
    textbox.style.width = width + 'px';
  }, 100);

  // Initiate the fade-in animation in after 100 milliseconds.
  // Setting it now will not trigger the CSS3 animation sequence.
  setTimeout(function() {
    qform.style.opacity = 1;
  }, 100);
}

// Create a centered pop-up.
var createCenteredPopup = function (query) {
  var windowX = (window.innerWidth - (PADDING_LEFT + options.frameWidth + PADDING_RIGHT)) / 2;
  var windowY = (window.innerHeight - (PADDING_TOP + options.frameHeight + PADDING_BOTTOM)) / 2;

  // Create new popup.
  var queries = expandQuery(query);
  executeQuery(queries, windowX, windowX, windowX, windowY, true);
}

// Create and fade in the dictionary popup frame and button.
var createPopup = function (queries, results, x, y, windowX, windowY, fixed) {
  // If an old frame still exists, wait until it is killed.
  var frame_ref = document.getElementById(ROOT_ID);
  if (frame_ref) {
    if (frame_ref.style.opacity == 1) {
      removePopup(true, false);
    }
    setTimeout(function() {createPopup(queries, results, x, y, windowX, windowY, fixed);}, 100);
    return;
  }

  // Render the results.
  var html = "";
  for (var i = 0; i < queries.length; i++) {
    var query = queries[i];
    var result = results[query];
    if (result) {
      html += "<div>"
           +    "<div>"
           +      "<h3>" + query + "</h3>"
           +      "<span>" + result["pron"].replace(/(?:\r\n|\r|\n)/g, "<br/>\n") + "</span>"
           +    "</div>"
           +    "<p>"  + result["trans"].replace(/(?:\r\n|\r|\n)/g, "<br/>\n") + "</p>"
           +  "</div>";
    }
  }

  if (html.length == 0) {
    return;
  }

  // Create the frame, set its id and insert it.
  var frame = document.createElement('div');
  frame.id = ROOT_ID;
  frame.innerHTML = html;

  // Unique class to differentiate between frame instances.
  frame.className = ROOT_ID + (new Date()).getTime();
  body.appendChild(frame);
  
  // Calculate frame position.
  var window_width = window.innerWidth;
  var window_height = window.innerHeight;
  var full_frame_width = PADDING_LEFT + options.frameWidth + PADDING_RIGHT;
  var full_frame_height = PADDING_TOP + options.frameHeight + PADDING_BOTTOM;
  var left = 0;
  var top = 0;
  var zoom_ratio = getZoomRatio();
  var body_margin_left = 0;
  var body_margin_top = 0;

  var body_margin_left_str = document.defaultView.getComputedStyle(body, null).getPropertyValue('margin-left');
  if (body_margin_left_str) {
    if (body_margin_left_str.match(/[.0-9]+px/)) {
      body_margin_left = parseFloat(body_margin_left_str);
    }
  }
  
  var body_margin_top_str = document.defaultView.getComputedStyle(body, null).getPropertyValue('margin-top');
  if (body_margin_top_str) {
    if (body_margin_top_str.match(/[.0-9]+px/)) {
      body_margin_top = parseFloat(body_margin_top_str);
    }
  }

  if (windowX + full_frame_width * zoom_ratio >= window_width) {
    left = x / zoom_ratio - full_frame_width;
    if (left < 0) left = 5;
  } else {
    left = x / zoom_ratio;
  }
  left = left - body_margin_left;
  
  if (windowY + full_frame_height * zoom_ratio + POPUP_SPACEY >= window_height) {
    top = y / zoom_ratio - full_frame_height - POPUP_SPACEY;
    if (top < 0) top = 5;
  } else {
    top = y / zoom_ratio + POPUP_SPACEY;
  }
  top = top - body_margin_top;

  
  // Set frame style.
  frame.style.position = fixed ? 'fixed' : 'absolute';
  frame.style.left = left + 'px';
  frame.style.top = top + 'px';
  frame.style.width = options.frameWidth + 'px';
  frame.style.height = options.frameHeight + 'px';
  frame.style.zIndex = BASE_Z_INDEX;
  
  // Create a dragging handle.
  handle = document.createElement('div');
  handle.id = ROOT_ID + '_handle';
  body.appendChild(handle);
  
  handle.style.position = fixed ? 'fixed' : 'absolute';
  handle.style.left = (left + options.frameWidth + PADDING_LEFT - 9) + 'px';
  handle.style.top = (top + options.frameHeight + PADDING_TOP + 3) + 'px';
  handle.style.background = 'url("' + HANDLE_ICON_URL + '")';
  handle.style.zIndex = BASE_Z_INDEX + 1;
  
  makeResizeable(frame, handle);
  
  // Make frame draggable by its top.
  makeMoveable(frame, PADDING_TOP);
  
  // Initiate the fade-in animation in after 10 milliseconds (100 mills in previous).
  // Setting it now will not trigger the CSS3 animation sequence.
  setTimeout(function() {
    frame.style.opacity = 1;
    handle.style.opacity = 1;
  }, 10);
}

// Fade out then destroy the frame and/or form.
var removePopup = function (do_frame, do_form) {
  var form = document.getElementById(FORM_ID);
  
  if (form && do_form) {
    grayOut(false);
    form.style.opacity = 0;
    setTimeout(function() {if (form) body.removeChild(form);}, 400);
  }
  
  // Remember the current frame's unique class name.
  var frame_ref = document.getElementById(ROOT_ID);
  var handle_ref = document.getElementById(ROOT_ID + '_handle');
  var frame_class = frame_ref ? frame_ref.className : null;
  
  if (frame_ref && do_frame) {
    frame_ref.style.opacity = 0;
    handle_ref.style.opacity = 0;
    setTimeout(function() {
      var frame_ref = document.getElementById(ROOT_ID);
      var handle_ref = document.getElementById(ROOT_ID + '_handle');
      // Check if the currently displayed frame is still the same as the old one.
      if (frame_ref && frame_ref.className == frame_class) {
        body.removeChild(frame_ref);
        body.removeChild(handle_ref);
      }
    }, 100);
  }
}

/***************************************************************
 *                   General Helper Functions                  *
 ***************************************************************/
// Background graying function, based on: 
// http://www.hunlock.com/blogs/Snippets:_Howto_Grey-Out_The_Screen
var grayOut = function (vis) {
  // Pass true to gray out screen, false to ungray.
  var dark_id = ROOT_ID + '_shader';
  var dark = document.getElementById(dark_id);
  var first_time = (dark == null);
  
  if (first_time) {
    // First time - create shading layer.
    var tnode = document.createElement('div');
    tnode.id = dark_id;
    
    tnode.style.position = 'absolute';
    tnode.style.top = '0px';
    tnode.style.left = '0px';
    tnode.style.overflow = 'hidden';
    
    document.body.appendChild(tnode);
    dark = document.getElementById(dark_id);
  }
  
  if (vis) {
    // Set the shader to cover the entire page and make it visible.
    dark.style.zIndex = BASE_Z_INDEX - 1;
    dark.style.backgroundColor = '#000000';
    dark.style.width = body.scrollWidth + 'px';
    dark.style.height = body.scrollHeight + 'px';
    dark.style.display = 'block';
    
    setTimeout(function() {dark.style.opacity = 0.7;}, 50);
  } else if (dark.style.opacity != 0) {
    setTimeout(function() {dark.style.opacity = 0;}, 50);
    setTimeout(function() {dark.style.display = 'none';}, 200);
  }
}

// Returns a trimmed version of the currently selected text.
var getTrimmedSelection = function () {
  var selection = String(window.getSelection());
  return selection.replace(/^\s+|\s+$/g, '');
}

// Returns the document body's zoom ratio.
var getZoomRatio = function () {
  var zoom_ratio = document.defaultView.getComputedStyle(body, null).getPropertyValue('zoom');
  return parseFloat(zoom_ratio || '0');
}

// Predicate to check whether the selected modifier key (and only it) is active
// in an event.
var checkModifier = function (modifier, e) {
  switch (modifier) {
    case 'None':
      return !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey;
    case 'Ctrl':
      return e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey;
    case 'Alt':
      return !e.ctrlKey && e.altKey && !e.metaKey && !e.shiftKey;
    case 'Meta':
      return !e.ctrlKey && !e.altKey && e.metaKey && !e.shiftKey;
    case 'Ctrl+Alt':
      return e.ctrlKey && e.altKey && !e.metaKey && !e.shiftKey;
    case 'Ctrl+Shift':
      return e.ctrlKey && !e.altKey && !e.metaKey && e.shiftKey;
    case 'Alt+Shift':
      return !e.ctrlKey && e.altKey && !e.metaKey && e.shiftKey;
    default:
      return false;
  }
}

// Makes a container resizeable through dragging a handle.
var makeResizeable = function (container, handle) {
  var last_position = {x: 0, y: 0};
  var ruler = document.createElement('div');
  ruler.style.visibility = 'none';
  ruler.style.width = '100px';

  function moveListener(e) {
    var moved = {x: (e.clientX - last_position.x),
                 y: (e.clientY - last_position.y)};

    var zoom_ratio = parseFloat(document.defaultView.getComputedStyle(ruler, null).getPropertyValue('width')) / 100;;
    var height = parseFloat(document.defaultView.getComputedStyle(container, null).getPropertyValue('height'));
    var width = parseFloat(document.defaultView.getComputedStyle(container, null).getPropertyValue('width'));
    var handle_left = parseFloat(document.defaultView.getComputedStyle(handle, null).getPropertyValue('left'));
    var handle_top = parseFloat(document.defaultView.getComputedStyle(handle, null).getPropertyValue('top'));

    var new_height = (height + moved.y) / zoom_ratio;
    var new_width = (width + moved.x) / zoom_ratio;
    var new_handle_left = (handle_left + moved.x) / zoom_ratio;
    var new_handle_top = (handle_top + moved.y) / zoom_ratio;

    if (moved.y > 0 || height >= 100) {
      last_position.y = e.clientY;
      container.style.height = new_height + 'px';
      handle.style.top = new_handle_top + 'px';
      
      if (options.saveFrameSize) {
        options.frameHeight = new_height;
        chrome.extension.sendRequest({method: 'store', arg: 'frameHeight', arg2: new_height}, function(response) {});
      }
    }
    if (moved.x > 0 || width >= 250) {
      last_position.x = e.clientX;
      container.style.width = new_width + 'px';
      handle.style.left = new_handle_left + 'px';
      
      if (options.saveFrameSize) {
        options.frameWidth = new_width;
        chrome.extension.sendRequest({method: 'store', arg: 'frameWidth', arg2: new_width}, function(response) {});
      }
    }
    
    e.preventDefault();
  }
  
  handle.addEventListener('mousedown', function(e) {
    last_position = {x: e.clientX, y: e.clientY};
    window.addEventListener('mousemove', moveListener);

    layer = document.createElement('div');
    body.appendChild(layer);
    layer.style.position = 'absolute';
    layer.style.top = '0px';
    layer.style.left = '0px';
    layer.style.width = '100%';
    layer.style.height = '100%';
    layer.style.opacity = '0';
    layer.style.zIndex = BASE_Z_INDEX+1;
    
    body.appendChild(ruler);
    
    window.addEventListener('mouseup', function(e) {
      window.removeEventListener('mousemove', moveListener);
      try {body.removeChild(ruler);} catch (e) {}
      try {body.removeChild(layer);} catch (e) {}
      e.preventDefault();
    });
    e.preventDefault();
  });
}

// Makes a box moveable by dragging its top margin.
var makeMoveable = function (box, margin) {
  var last_position = {x: 0, y: 0};

  function moveListener(e) {
    var moved = {x: (e.clientX - last_position.x),
                 y: (e.clientY - last_position.y)};
    last_position = {x: e.clientX, y: e.clientY};
    
    box.style.top = (box.offsetTop + moved.y) + 'px';
    box.style.left = (box.offsetLeft + moved.x) + 'px';
    
    var handle = document.getElementById(ROOT_ID + '_handle');
    handle.style.top = (handle.offsetTop + moved.y) + 'px';
    handle.style.left = (handle.offsetLeft + moved.x) + 'px';
    
    e.preventDefault();
  }

  box.addEventListener('mousedown', function(e) {
    var y = box.offsetTop;
    var zoom_ratio = getZoomRatio();
    var mouse_y = e.pageY / zoom_ratio;
    if (mouse_y >= y && mouse_y <= y + margin * zoom_ratio) {
      last_position = {x: e.clientX, y: e.clientY};
      
      layer = document.createElement('div');
      body.appendChild(layer);
      layer.style.position = 'absolute';
      layer.style.top = '0px';
      layer.style.left = '0px';
      layer.style.width = '100%';
      layer.style.height = '100%';
      layer.style.opacity = '0';
      layer.style.zIndex = BASE_Z_INDEX+1;
      
      window.addEventListener('mousemove', moveListener);
      window.addEventListener('mouseup', function(e) {
        window.removeEventListener('mousemove', moveListener);
        try {body.removeChild(layer);} catch (e) {}
        e.preventDefault();
      });
      e.preventDefault();
    }
  });
}

// Checks whether a click event was inside the current popup frame.
var isClickInsideFrame = function (e) {
  frame_ref = document.getElementById(ROOT_ID);
  
  if (frame_ref) {
    if (frame_ref.style.position == 'absolute') {
      var x = e.pageX;
      var y = e.pageY;
    } else if (frame_ref.style.position == 'fixed') {
      var x = e.clientX;
      var y = e.clientY;
    }
    
    var zoom_ratio = getZoomRatio();
    x /= zoom_ratio;
    y /= zoom_ratio;
    
    if (x >= frame_ref.offsetLeft - options.inframeMargin &&
        x <= frame_ref.offsetLeft + frame_ref.offsetWidth + options.inframeMargin &&
        y >= frame_ref.offsetTop  - options.inframeMargin&&
        y <= frame_ref.offsetTop + frame_ref.offsetHeight + options.inframeMargin) {
      return true;
    }
  }
  
  return false;
}

// Encodes a string as UTF-8.
var utf8encode = function (string) {
  string = string.replace(/\r\n/g,"\n");
  var utftext = "";

  for (var n = 0; n < string.length; n++) {

    var c = string.charCodeAt(n);

    if (c < 128) {
      utftext += String.fromCharCode(c);
    }
    else if ((c > 127) && (c < 2048)) {
      utftext += String.fromCharCode((c >> 6) | 192);
      utftext += String.fromCharCode((c & 63) | 128);
    }
    else {
      utftext += String.fromCharCode((c >> 12) | 224);
      utftext += String.fromCharCode(((c >> 6) & 63) | 128);
      utftext += String.fromCharCode((c & 63) | 128);
    }

  }

  return utftext;
}

/******************************************************************************/
initialize();
