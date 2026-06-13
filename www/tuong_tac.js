
      $(document).on('shiny:value', function(e) {
        if (e.name === 'tabs') {
          var tab = e.value;
          $('.app-nav-link').removeClass('active');
          $('#nav_' + tab).addClass('active');
        }
      });
      // Cap nhat nav active ngay khi bam.
      $(document).on('click', '.app-nav-link', function() {
        $('.app-nav-link').removeClass('active');
        $(this).addClass('active');
      });
      // Dat active mac dinh khi ket noi Shiny.
      $(document).on('shiny:connected', function() {
        $('#nav_overview').addClass('active');
      });
      var dashboardResizeTimer = null;
      function resizeDashboardWidgets(shouldNotifyWindow) {
        clearTimeout(dashboardResizeTimer);
        dashboardResizeTimer = setTimeout(function() {
          if (shouldNotifyWindow) {
            window.dispatchEvent(new Event('resize'));
          }
          if (window.Plotly) {
            $('.js-plotly-plot').each(function() {
              Plotly.Plots.resize(this);
            });
          }
        }, 120);
      }
      $(document).on('click', '.app-nav-link', function() { resizeDashboardWidgets(true); });
      $(document).on('shiny:value', function() { resizeDashboardWidgets(false); });
      $(window).on('resize', function() { resizeDashboardWidgets(false); });
      function scrollAssistantChat() {
        setTimeout(function() {
          var logs = document.querySelectorAll('.gemini-chat-container');
          logs.forEach(function(log) { log.scrollTop = log.scrollHeight; });
        }, 80);
      }
      function streamReveal(element) {
        var $el = $(element);
        var children = $el.find('p, li, .assistant-answer-grid > div, .assistant-listing, .assistant-insight, table tr');
        if (children.length === 0) {
          children = $el.children();
        }
        if (children.length === 0) {
          children = $el;
        }
        
        children.css({
          'opacity': '0',
          'transform': 'translateY(10px)',
          'transition': 'opacity 0.5s ease-out, transform 0.5s ease-out'
        });
        
        children.each(function(index) {
          var child = this;
          setTimeout(function() {
            $(child).css({
              'opacity': '1',
              'transform': 'translateY(0)'
            });
            scrollAssistantChat();
          }, index * 200);
        });
      }
      $(document).on('shiny:value', function(e) {
        if (e.name === 'gemini_chat_view' || e.name === 'assistant_chat') {
          scrollAssistantChat();
          setTimeout(function() {
            var latestBotText = $('.gemini-message.bot.typing .gemini-text').last();
            if (latestBotText.length > 0 && !latestBotText.hasClass('streamed')) {
              latestBotText.addClass('streamed');
              streamReveal(latestBotText);
            }
          }, 100);
        }
      });
      $(document).on('keydown', '#assistant_question', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          $('#assistant_send').click();
        }
      });

      // Giong noi - nhan dien giong noi (STT).
      var SpeechRecognition = window.SpeechRecognition || window.webkitSpeechRecognition;
      var recognition = null;
      var isListening = false;
      var recognitionTimeout = null;

      if (SpeechRecognition) {
        recognition = new SpeechRecognition();
        recognition.lang = 'vi-VN';
        recognition.interimResults = true;
        recognition.continuous = true;

        recognition.onstart = function() {
          isListening = true;
          $('#assistant_mic').addClass('listening').html('<i class="fa fa-circle-dot" style="color: #ea4335; animation: geminiMicPulse 1s infinite;"></i>');
        };

        recognition.onresult = function(event) {
          clearTimeout(recognitionTimeout);
          var finalTranscript = '';
          for (var i = event.resultIndex; i < event.results.length; ++i) {
            if (event.results[i].isFinal) {
              finalTranscript += event.results[i][0].transcript;
            } else {
              // Hien ket qua tam thoi khi trinh duyet tra ve.
              var interim = event.results[i][0].transcript;
              if (interim !== '') {
                $('#assistant_question').val(interim);
              }
            }
          }
          if (finalTranscript !== '') {
            $('#assistant_question').val(finalTranscript).trigger('change');
            
            // Tu dung sau 2 giay im lang.
            recognitionTimeout = setTimeout(function() {
              recognition.stop();
            }, 2000);
          }
        };

        recognition.onerror = function(event) {
          console.error('Speech recognition error:', event.error);
          recognition.stop();
        };

        recognition.onend = function() {
          isListening = false;
          $('#assistant_mic').removeClass('listening').html('<i class="fa fa-microphone"></i>');
          clearTimeout(recognitionTimeout);
        };
      }

      $(document).on('click', '#assistant_mic', function(e) {
        e.preventDefault();
        if (!recognition) {
          alert('Trình duyệt của bạn không hỗ trợ nhận diện giọng nói. Hãy dùng Google Chrome hoặc Safari.');
          return;
        }
        if (isListening) {
          recognition.stop();
        } else {
          $('#assistant_question').val('').trigger('change');
          recognition.start();
        }
      });

      // Giong noi - doc cau tra loi (TTS).
      window.speakText = function(btn) {
        var $btn = $(btn);
        var botContainer = $btn.closest('.gemini-bot-container');
        // Lay text thuan, bo qua cum nut thao tac.
        var textToSpeak = botContainer.find('.gemini-text').text().trim();
        
        if (window.speechSynthesis.speaking && $btn.hasClass('speaking')) {
          window.speechSynthesis.cancel();
          $btn.removeClass('speaking');
          return;
        }
        
        window.speechSynthesis.cancel();
        $('.speak-btn').removeClass('speaking');
        
        var utterance = new SpeechSynthesisUtterance(textToSpeak);
        utterance.lang = 'vi-VN';
        
        var voices = window.speechSynthesis.getVoices();
        var viVoice = voices.find(function(v) { return v.lang.indexOf('vi') > -1; });
        if (viVoice) utterance.voice = viVoice;
        
        $btn.addClass('speaking');
        
        utterance.onend = function() {
          $btn.removeClass('speaking');
        };
        utterance.onerror = function() {
          $btn.removeClass('speaking');
        };
        
        window.speechSynthesis.speak(utterance);
      };

      if (window.speechSynthesis) {
        window.speechSynthesis.getVoices();
        if (window.speechSynthesis.onvoiceschanged !== undefined) {
          window.speechSynthesis.onvoiceschanged = function() {
            window.speechSynthesis.getVoices();
          };
        }
      }
