// Populates the <select> tags from the data.json file
$.getJSON("/json/data.json", function (data) {
    var $selectEyesight = $('select[name="inputEyesight"]');
    var $selectHearing = $('select[name="inputHearing"]');
  
    console.log(data);
    $selectEyesight.html('');
    $selectHearing.html('');
    
    for (var i = 0; i < data['eyesight'].length; i++) {
      $selectEyesight.append('<option id="' + data['eyesight'][i]['id'] + '" value="' + data['eyesight'][i]['value'] + '">' + data['eyesight'][i]['name'] + '</option>');
    }
  
    for (var i = 0; i < data['hearing'].length; i++) {
      $selectHearing.append('<option id="' + data['hearing'][i]['id'] + '" value="' + data['hearing'][i]['value'] + '">' + data['hearing'][i]['name'] + '</option>');
    }
  })
  .fail(function (jqxhr, status, error) { 
    console.log('error', status, error);
  });
  
  // RANDOM NUMBER GENERATOR
  function randomIntFromInterval(min,max) {
      return Math.floor(Math.random()*(max-min+1)+min);
  }
  
  // ADDS ARRAY ELEMENTS
  function add(a, b) {
    return a + b;
  }
  
  // CLEAR MODAL
  function clearResults() {
    $( '#theResults' ).empty();
  }
  
  // ROLL STATS
  // TODO
  function rollStats() {
    var statResults = new Array();
    var roll = new Array();
    
    for (var i = 0; i < 4; i++) {
      // Roll 4D6
      roll[0] = randomIntFromInterval(1,6);
      roll[1] = randomIntFromInterval(1,6);
      roll[2] = randomIntFromInterval(1,6);
      roll[3] = randomIntFromInterval(1,6);
      console.log('Iteration: ' + i + ' Roll: ' + roll);
      // Remove lowest number
      var min = Math.min(...roll);
      for (var ii = 0; ii < roll.length; ii++) {
          if (roll[ii] === min) {
            roll.splice(ii, 1);
        break;
        }
      }
      // Add remaining rolls
      statResults[i] = roll.reduce(add, 0);
    }
    console.log('statResults: ' + statResults);
    return statResults;
  }
  
  // MODAL START
  var modal = document.getElementById('resultsModal');
  var span = document.getElementsByClassName('close')[0];
  
  // When the user clicks on <span> (x), close the modal
  span.onclick = function(){
    modal.style.display = 'none';
    clearResults();
  };
  
  // When the user clicks anywhere outside of the modal, close it
  window.onclick = function(event) {
    if (event.target == modal) {
      modal.style.display = 'none';
      clearResults();
    }
  };
  
  function generateModal(){
    modal.style.display = 'block';
  }
  
  function populateModal(submitted, convertedStats, veteranPoints, eyesightModifier, hearingModifier){
    $('#theResults').append(submitted[0]['value'] + '\'s Harnmaster stats are as follows: <br/>')
    $('#theResults').append('Dexterity or Agility: ' + convertedStats[0] + '<br/>');
    $('#theResults').append('Stamina: '+ convertedStats[1] + '<br/>');
    $('#theResults').append('Strength: ' + convertedStats[2] + '<br/>');
    $('#theResults').append('Aura: ' + convertedStats[3] + '<br/>');
    $('#theResults').append('Itelligence: ' + convertedStats[4] + '<br/>');
    $('#theResults').append('Voice: ' + convertedStats[5] + '<br/>');
    $('#theResults').append('Comeliness: ' + convertedStats[6] + '<br/>');
    $('#theResults').append('Will: ' + convertedStats[7] + '<br/>');
    if (submitted[16]['value'] === 'true') {
      var rolledStats = rollStats();
        $('#theResults').append('Hearing: ' + rolledStats[0] + ' ' + hearingModifier + '<br/>');
        $('#theResults').append('Eyesight: ' + rolledStats[1] + ' ' + eyesightModifier + '<br/>');
        $('#theResults').append('Smell: ' + rolledStats[2] + '<br/>');
        $('#theResults').append('Morality: ' + rolledStats[3] + '<br/>');
    } else {
      $('#theResults').append('Hearing: ' + hearingModifier + '<br/>');
      $('#theResults').append('Eyesight: ' + eyesightModifier + '<br/>');
    }
    $('#theResults').append('Veteran Points: ' + veteranPoints + '<br/>');
  }
  // MODAL END
  
  // START CHARACTER CONVERSION
  function findCSR (stat, averageCPRS) {
    if (stat >= 17) {
      stat = stat + (stat - 16);
    }
    stat = stat / averageCPRS;
    stat = Number(stat).toFixed(4);
    return stat;
  }
  
  function recordCSR ( data, averageCPRS ) {
    var dex = Number(data[3]['value']);
    var con = Number(data[4]['value']);
    var str = Number(data[5]['value']);
    var wis = Number(data[6]['value']);
    var int = Number(data[7]['value']);
    var bv = Number(data[8]['value']);
    var app = Number(data[9]['value']);
    var fort = Number(data[10]['value']);
    var csrs = new Array(); //Shorthand for Characteristic Stat Ratios
    
    //dex
    dex = findCSR(dex, averageCPRS);
    csrs[0] = dex;
    
    //con
    con = findCSR(con, averageCPRS);
    csrs[1] = con;
    
    //str
    str = findCSR(str, averageCPRS);
    csrs[2] = str;
    
    //wis
    wis = findCSR(wis, averageCPRS);
    csrs[3] = wis;
    
    //int
    int = findCSR(int, averageCPRS);
    csrs[4] = int;
    
    //bv
    bv = findCSR(bv, averageCPRS);
    csrs[5] = bv;
    
    //app
    app = findCSR(app, averageCPRS);
    csrs[6] = app;
    
    //fort
    fort = findCSR(fort, averageCPRS);
    csrs[7] = fort;
    
    return csrs;
  }
  
  function buildHarnCharacter(originalCPRS, csr){
    var harnCharacteristics = new Array();
    
    for (var i = 0; i < csr.length; i++){
      harnCharacteristics[i] = (csr[i] * originalCPRS);
    }
    
    return harnCharacteristics;
  }
  
  function convertCharacter( data ) {
    var totalCPRS = 0;
    var averageCPRS = 0;
    var actualCost = 0;
    var csr; //Shorthand for Characteristic Stat Ratio
    var originalCPRS = 0;
    var harnCharacteristics;
    
    for (var i = 3; i < 12; i++) {
      if (Number(data[i]['value']) >= 17) {
        actualCost = actualCost + (Number(data[i]['value']) + (Number(data[i]['value'] - 16)));
      } else {
        actualCost = actualCost + Number(data[i]['value']);
      }
    }
    
    for (var i = 3; i < 12; i++) {
      totalCPRS = totalCPRS + Number(data[i]['value']);
    }
    
    averageCPRS = totalCPRS / 9;
    
    csr = recordCSR(data, averageCPRS);
    
    if (data[1]['value'] === "well") {
      originalCPRS = (totalCPRS - (totalCPRS * 0.05) - 50) / 10;
    } else {
      originalCPRS = (totalCPRS - 50) / 10;
    }
    
    harnCharacteristics = buildHarnCharacter(originalCPRS, csr);
    
    console.log('totalCPRS: ' + totalCPRS);
    console.log('averageCPRS: ' + averageCPRS);
    console.log('actualCost: ' + actualCost);
    console.log('CSR: ' + csr);
    console.log('originalCPRS: ' + originalCPRS);
    console.log('harnCharacteristics: ' + harnCharacteristics);
    
    return harnCharacteristics;
  }
  
  function eyesightConversion(submitted){
    var eyeValue = submitted[13]['value'];
    var eyeMod = '';
    
    if (eyeValue === 'eye0') {
      eyeMod = '+1';
    } else if (eyeValue === 'eye1') {
      eyeMod = '0';
    } else if (eyeValue === 'eye2') {
      eyeMod = '-1';
    } else if (eyeValue === 'eye3') {
      eyeMod = '-3';
    } else if (eyeValue === 'eye4') {
      eyeMod = '-5';
    } else if (eyeValue === 'eye8') {
      eyeMod = '+3';
    } else {
      eyeMod = 'Colorblind stays, +2 to ANOTHER stat';
    }
    
    console.log('eyeMod: ' + eyeMod);
    return eyeMod;
  }
  
  function hearingConversion(submitted){
    var hearValue = submitted[12]['value'];
    var hearMod = '';
    
    if (hearValue === 'hear0') {
      hearMod = '-2';
    } else if (hearValue === 'hear1') {
      hearMod = '-1';
    } else if (hearValue === 'hear3') {
      hearMod = '+1';
    } else if (hearValue === 'hear4') {
      hearMod = '+2';
    } else {
      hearMod = '0';
    }
    
    console.log('hearMod: ' + hearMod);
    return hearMod;
  }
  
  function convertVeteranPoints(submitted){
    var level = Number(submitted[2]['value']);
    var veteranPoints = 0;
    
    if (level >= 9) {
      veteranPoints = level + (level - 8);
    } else {
      veteranPoints = level;
    }
    
    console.log('veteranPoints: ' + veteranPoints);
    
    return veteranPoints;
  }
  // END CHARACTER CONVERSION
  
  // Runs when the submit button is pressed for cands_to_harn.html
  function cands_to_harnSubmit() {
    $('#inputCharacter').unbind('submit').bind('submit',function(event) {
      var submitted = $( this ).serializeArray();
        
        console.log( submitted );
        event.preventDefault();
        
        var convertedStats = convertCharacter(submitted);
        var eyesightModifier = eyesightConversion(submitted);
        var hearingModifier = hearingConversion(submitted);
        var veteranPoints = convertVeteranPoints(submitted);
        
        if (submitted[14]['value'] === 'add') {
          for (var i = 0; i < convertedStats.length; i++) {
            convertedStats[i] = convertedStats[i] + 3.5;
          }
        } else if (submitted[14]['value'] === 'roll') {
          for (var i = 0; i < convertedStats.length; i++) {
            var roll = randomIntFromInterval(1, 6);
            convertedStats[i] = convertedStats[i] + roll;
          }
        }
        
        if (submitted[15]['value'] === 'true') {
          convertedStats[3] = convertedStats[3] + 3;
        }
        generateModal();
        clearResults();
        populateModal(submitted, convertedStats, veteranPoints, eyesightModifier, hearingModifier);
    });
  }