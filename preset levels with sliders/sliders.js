function updateDelayedValue(el, sliderInputBinding) {
  $(el).data('sliderDelayedValue', sliderInputBinding.getValue(el));
}

(function() {
  var sliderInputBinding = Shiny.inputBindings.bindingNames['shiny.sliderInput'].binding;

  var liveSliderInputBinding = $.extend({}, sliderInputBinding, {
    find: function(scope) {
      return $(scope).find(".slider-value-live");
    },
    subscribe: function(el, callback) {
      // A live slider simply always suppresses the rate policy
      sliderInputBinding.subscribe(el, function(useRatePolicy) {
        callback(false);
      });
    }
  });

  var delayedSliderInputBinding = $.extend({}, sliderInputBinding, {
    find: function(scope) {
      return $(scope).find(".slider-value-delayed");
    },
    subscribe: function(el, callback) {
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');

      // Ignore the update message that the slider normally subscribes to, and only pay attention to the finish message, which is sent when the user releases the slider handle
      slider.update({
        onFinish: function() {
          callback(false);
        }
      });
    },
    unsubscribe: function(el) {
      slider.update({
        onFinish: null
      });
    }
  });

  var dualSliderInputBinding = $.extend({}, sliderInputBinding, {
    find: function(scope) {
      return $(scope).find(".slider-value-both");
    },
    getValue: function(el) {
      // The value of a dual slider is a pair (live value, delayed value). When the user is dragging the slider handle, the live value tracks the handle, and the delayed value is stuck at the pre-drag state of the handle; when the user releases the slider handle, the delayed value updates to match the live value.
      var liveValue = sliderInputBinding.getValue(el);
      var delayedValue = $(el).data('sliderDelayedValue');
      if (typeof delayedValue === "undefined") {
        updateDelayedValue(el, sliderInputBinding);
        return {live: liveValue, delayed: liveValue};
      } else {
        return {live: liveValue, delayed: delayedValue};
      }
    },
    subscribe: function(el, callback) {
      // A dual slider always suppresses the rate policy; if you need to debounce or throttle the live value, you need to do it yourself
      sliderInputBinding.subscribe(el, function(useRatePolicy) {
        callback(false);
      });
      
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');

      // Subscribe to the finish message on the slider, and update the delayed value when the user releases the slider handle
      slider.update({
        onFinish: function() {
          updateDelayedValue(el, sliderInputBinding);
          callback(false);
        }
      });
    },
    setValue: function(el, value) {
      // When setting the value directly, update both live and delayed value
      sliderInputBinding.setValue(el, value);
      updateDelayedValue(el, sliderInputBinding);
    },
    receiveMessage: function(el, data) {
      // When handling a message, update both live and delayed value
      sliderInputBinding.receiveMessage(el, data);
      updateDelayedValue(el, sliderInputBinding);
    },
    unsubscribe: function(el) {
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');

      slider.update({
        onFinish: null
      });
    }
  });

  Shiny.inputBindings.register(liveSliderInputBinding, 'custom.liveSliderInput');
  Shiny.inputBindings.register(delayedSliderInputBinding, 'custom.delayedSliderInput');
  Shiny.inputBindings.register(dualSliderInputBinding, 'custom.dualSliderInput');
  Shiny.inputBindings.setPriority("custom.liveSliderInput", 10);
  Shiny.inputBindings.setPriority("custom.delayedSliderInput", 10);
  Shiny.inputBindings.setPriority("custom.dualSliderInput", 10);

  var inputsInitialized = false;
  $(document).one('shiny:connected', function() {
    inputsInitialized = true;
  });
})();
