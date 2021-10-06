$(document).ready(function(){
  $('[data-toggle="tooltip"]').tooltip();
});

$(document).on('click', '.set-brush', function(){
  var button = $(this);
  var plotId = button.data('plot-id');
  var eventId = button.data('event-id');
  var panelId = button.data('panel-id');

  if (plotId && eventId){ //both attributes must be present
    var plot = $('#' + plotId);

    if (plot){
      var panelSelect = $('#' + panelId);
      var widthOffset = 0.5 * plot.width();
      var heightOffset = 0.5 * plot.height();

      if (panelSelect.length){
        var positions = JSON.parse(panelSelect.closest('.panel-selection').children()[1].innerHTML);
        var panels = Object.keys(positions);
        var value = panelSelect.val();

        if (panels.length && panels.includes(value)){
          var selected = positions[value];
          widthOffset = selected.x;
          heightOffset = selected.y;
        }
      }
      var offset = plot.offset();
      // force shiny to send plot info (CSS mapping) to the backend
      // this is something that cannot be retrieved from the backend
      plot.trigger({
        type: 'dblclick2.image_output', // double click on plot event
        which: 1,
        pageX: offset.left + (widthOffset / window.devicePixelRatio),
        pageY: offset.top + (heightOffset / window.devicePixelRatio)
      });

      Shiny.onInputChange(eventId, new Date()); // trigger input
    }
  }
});

$(document).on('shiny:error', '.downloadable-plot.ready .tooltip-plot-output, .downloadable-plot.ready .shiny-plot-output', function(evt){
  $('#' + evt.target.id).closest('.downloadable-plot').removeClass('ready');
});
$(document).on('shiny:value', '.downloadable-plot:not(.ready) .tooltip-plot-output, .downloadable-plot:not(.ready) .shiny-plot-output', function(evt){
  $('#' + evt.target.id).closest('.downloadable-plot').addClass('ready');
});

function setPlotBrush(outputId, startX, endX, startY, endY){
  var plot = $('#' + outputId);
  var brushId = plot.data('brush-id');

  if (brushId){
    Shiny.resetBrush(brushId);

    if (startX < 0 || endX < 0 || startY < 0 || endY < 0){
      // requested coordinates are outside of the domain
      return;
    }

    var offset = plot.offset();
    startX += offset.left;
    endX += offset.left;
    startY += offset.top;
    endY += offset.top;

    plot.trigger({ // click in the start position
      type: 'mousedown.image_output',
      which: 1,
      pageX: startX,
      pageY: startY
    });
    $(document).trigger({ // move to the end position
      type: 'mousemove.image_brush',
      which: 1,
      pageX: endX,
      pageY: endY
    });
    $(document).trigger({ // release the click
      type: 'mouseup.image_brush',
      which: 1,
      pageX: endX,
      pageY: endY
    });

    // shiny will send the brush area info to the backend
  }
}

// data tables
function preventLinkSelections(row){
  $(row).find('a').each(function(){
    $(this).mousedown(function(e){
      e.preventDefault();
      e.stopPropagation();
      return false;
    });
  });
}

function buildDropdownLinkMenu(itemDefs, label){
  var id = generateId();
  var items = itemDefs.map(function(item){
    return '<li role="presentation"><a role="menuitem" tabindex="-1" href="' + item.url + '" target="_blank">' + item.label + '</a></li>';
  });

  var a = '<span class="caret"></span>';
  var l = '<a class="dropdown-toggle" type="button" id="' + id + '" data-toggle="dropdown">' + label + a + '</a>';
  var u = '<ul class="dropdown-menu" role="menu" aria-labelledby="' + id + '">' + items.join('') + '</ul>';

  return '<div class="dropdown">' + l + u + '<div>';
}

function generateId(){
  return '_' + Math.random().toString(36).substr(2, 9);
}

function ensgRowCallback(row, data, species, idx){
  if (idx === undefined){
    idx = 0;
  }
  
  if(!Array.isArray(idx)) {
    idx = [idx]
  }
  
  for(const ii of idx) {
    var ensg = data[ii];
    var itemDefs = [
      {
        label: 'Ensembl',
        url: 'https://www.ensembl.org/' + species + '/Gene/Summary?db=core;g=' + ensg
      },
      {
        label: 'NCBI',
        url: 'https://www.ncbi.nlm.nih.gov/gene/?term=' + ensg
      }
    ];
    $(row).find('td').eq(ii).html(buildDropdownLinkMenu(itemDefs, ensg));
  }

  preventLinkSelections(row);
}
