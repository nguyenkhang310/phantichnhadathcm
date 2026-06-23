
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
      function isMobileNavViewport() {
        return (window.innerWidth || document.documentElement.clientWidth || 1024) <= 900;
      }
      function setMobileMenu(open) {
        var $sidebar = $('.app-sidebar');
        var $toggle = $('#mobile_menu_toggle');
        $sidebar.toggleClass('mobile-menu-open', open);
        $('body').toggleClass('mobile-menu-locked', open && isMobileNavViewport());
        $toggle.attr('aria-expanded', open ? 'true' : 'false');
        $toggle.attr('aria-label', open ? 'Đóng menu điều hướng' : 'Mở menu điều hướng');
        $toggle.html(open ? '<i class="fa fa-xmark"></i>' : '<i class="fa fa-bars"></i>');
      }
      $(document).on('click', '#mobile_menu_toggle', function(e) {
        e.preventDefault();
        e.stopPropagation();
        setMobileMenu(!$('.app-sidebar').hasClass('mobile-menu-open'));
      });
      $(document).on('click', '.app-sidebar-nav', function(e) {
        e.stopPropagation();
      });
      $(document).on('click', '.app-nav-link', function() {
        if (isMobileNavViewport()) setMobileMenu(false);
      });
      $(document).on('click', function() {
        if (isMobileNavViewport()) setMobileMenu(false);
      });
      $(document).on('keydown', function(e) {
        if (e.key === 'Escape') setMobileMenu(false);
      });
      function clampNumber(value, min, max) {
        return Math.max(min, Math.min(max, value));
      }
      function setCssPx(name, value) {
        document.documentElement.style.setProperty(name, Math.round(value) + 'px');
      }
      function updateViewportFit() {
        var root = document.documentElement;
        var width = window.innerWidth || root.clientWidth || 1366;
        var height = window.innerHeight || root.clientHeight || 768;
        var compact = width < 1320 || height < 760;
        var ultraCompact = width < 1100 || height < 650;
        var mobile = width <= 900;
        var smallMobile = width <= 640;
        var spaceScale = clampNumber(Math.min(width / 1440, height / 840), 0.62, 1);
        var fontScale = clampNumber(Math.min(width / 1280, height / 760), 0.84, 1);

        if (mobile) {
          spaceScale = clampNumber(width / 520, 0.66, 0.9);
          fontScale = clampNumber(width / 520, 0.88, 1);
        }
        if (smallMobile) {
          spaceScale = clampNumber(width / 460, 0.64, 0.82);
        }

        root.classList.toggle('fit-compact', compact || mobile);
        root.classList.toggle('fit-ultra-compact', ultraCompact);

        setCssPx('--fit-sidebar-width', mobile ? width : clampNumber(260 * spaceScale, 214, 260));
        setCssPx('--fit-topbar-height', compact || mobile ? 64 : 72);
        setCssPx('--fit-topbar-pad-x', clampNumber(32 * spaceScale, 12, 32));
        setCssPx('--fit-topbar-pad-y', compact || mobile ? clampNumber(10 * spaceScale, 8, 10) : 0);
        setCssPx('--fit-page-pad-x', clampNumber(40 * spaceScale, smallMobile ? 12 : 18, 40));
        setCssPx('--fit-page-pad-y', clampNumber(32 * spaceScale, smallMobile ? 12 : 16, 32));
        setCssPx('--fit-page-pad-bottom', clampNumber(48 * spaceScale, 18, 48));
        setCssPx('--fit-panel-gap', clampNumber(24 * spaceScale, 12, 24));
        setCssPx('--fit-grid-gap', clampNumber(24 * spaceScale, 10, 24));
        setCssPx('--fit-panel-pad', clampNumber(24 * spaceScale, 12, 24));
        setCssPx('--fit-panel-head-x', clampNumber(24 * spaceScale, 12, 24));
        setCssPx('--fit-panel-head-y', clampNumber(20 * spaceScale, 10, 20));
        setCssPx('--fit-kpi-pad', clampNumber(24 * spaceScale, 12, 24));
        setCssPx('--fit-kpi-foot-gap', clampNumber(16 * spaceScale, 8, 16));
        setCssPx('--fit-title-font', clampNumber(28 * fontScale, smallMobile ? 20 : 22, 28));
        setCssPx('--fit-subtitle-font', clampNumber(15 * fontScale, 12, 15));
        setCssPx('--fit-topbar-title-font', clampNumber(18 * fontScale, 14, 18));
        setCssPx('--fit-kpi-value-font', clampNumber(34 * fontScale, 24, 34));
        setCssPx('--fit-kpi-text-font', clampNumber(22 * fontScale, 17, 22));
        setCssPx('--fit-kpi-median-font', clampNumber(25 * fontScale, 18, 25));
        setCssPx('--fit-form-font', clampNumber(14 * fontScale, 12, 14));
        setCssPx('--fit-nav-font', clampNumber(14 * fontScale, 12, 14));
        setCssPx('--fit-nav-height', clampNumber(44 * spaceScale, 34, 44));
        setCssPx('--fit-nav-pad-x', clampNumber(16 * spaceScale, 10, 16));
        setCssPx('--fit-nav-margin-x', clampNumber(16 * spaceScale, 10, 16));
        setCssPx('--fit-nav-margin-y', clampNumber(4 * spaceScale, 2, 4));
        setCssPx('--fit-brand-pad-top', clampNumber(32 * spaceScale, 12, 32));
        setCssPx('--fit-brand-pad-x', clampNumber(20 * spaceScale, 12, 20));
        setCssPx('--fit-brand-pad-bottom', clampNumber(24 * spaceScale, 10, 24));
        setCssPx('--fit-brand-gap', clampNumber(16 * spaceScale, 8, 16));
        setCssPx('--fit-logo-size', mobile ? 64 : clampNumber(120 * spaceScale, 64, 120));
        setCssPx('--fit-footer-pad', clampNumber(24 * spaceScale, 10, 24));
        setCssPx('--fit-filter-min', clampNumber(170 * spaceScale, 128, 170));
        setCssPx('--fit-filter-gap', clampNumber(20 * spaceScale, 10, 20));
        setCssPx('--fit-widget-min', clampNumber(height * (ultraCompact ? 0.31 : 0.34), ultraCompact ? 190 : 230, 300));
        setCssPx('--fit-plot-regular', clampNumber(height * 0.36, ultraCompact ? 220 : 260, 340));
        setCssPx('--fit-plot-medium', clampNumber(height * 0.42, ultraCompact ? 250 : 300, 390));
        setCssPx('--fit-plot-large', clampNumber(height * 0.47, ultraCompact ? 280 : 320, 460));
        setCssPx('--fit-plot-small', clampNumber(height * 0.30, 200, 280));
        setCssPx('--fit-plot-xlarge', clampNumber(height * 0.54, ultraCompact ? 300 : 360, 500));
        setCssPx('--fit-map-height', clampNumber(height - (compact ? 240 : 190), ultraCompact ? 300 : 380, 640));
        setCssPx('--fit-gemini-title', clampNumber(44 * fontScale, smallMobile ? 26 : 30, 44));
        setCssPx('--fit-gemini-card-height', clampNumber(140 * spaceScale, 104, 140));
        setCssPx('--fit-gemini-grid-margin', clampNumber(48 * spaceScale, 18, 48));

        window.setTimeout(function() {
          var topbar = document.querySelector('.app-topbar');
          var topbarHeight = topbar ? Math.ceil(topbar.getBoundingClientRect().height) : (compact || mobile ? 64 : 72);
          setCssPx('--fit-topbar-actual', topbarHeight);
        }, 0);
        if (!mobile) setMobileMenu(false);
      }
      var dashboardResizeTimer = null;
      function resizeDashboardWidgets(shouldNotifyWindow) {
        clearTimeout(dashboardResizeTimer);
        dashboardResizeTimer = setTimeout(function() {
          updateViewportFit();
          if (shouldNotifyWindow) {
            window.dispatchEvent(new Event('resize'));
          }
          if (window.Plotly) {
            $('.js-plotly-plot').each(function() {
              var compact = document.documentElement.classList.contains('fit-compact');
              var leftMargin = clampNumber(this.clientWidth * 0.14, compact ? 56 : 80, compact ? 112 : 140);
              var bottomMargin = clampNumber(this.clientHeight * 0.18, compact ? 48 : 64, compact ? 78 : 96);
              try {
                Plotly.relayout(this, {
                  'margin.l': Math.round(leftMargin),
                  'margin.r': compact ? 16 : 24,
                  'margin.t': compact ? 12 : 20,
                  'margin.b': Math.round(bottomMargin),
                  'legend.font.size': compact ? 10 : 11,
                  'xaxis.tickfont.size': compact ? 9 : 10,
                  'yaxis.tickfont.size': compact ? 10 : 11
                });
                Plotly.Plots.resize(this);
              } catch (err) {
                try {
                  Plotly.Plots.resize(this);
                } catch (resizeErr) {}
              }
            });
          }
          $('.leaflet.html-widget, .leaflet-container').each(function() {
            if (this._leaflet_map && this._leaflet_map.invalidateSize) {
              this._leaflet_map.invalidateSize(false);
            }
          });
        }, 120);
      }
      $(document).on('click', '.app-nav-link', function() { resizeDashboardWidgets(true); });
      $(document).on('shiny:value', function() { resizeDashboardWidgets(false); });
      $(document).on('shiny:connected', function() { resizeDashboardWidgets(true); });
      $(window).on('resize orientationchange', function() { resizeDashboardWidgets(false); });
      updateViewportFit();
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
