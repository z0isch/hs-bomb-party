(function () {
  htmx.defineExtension("game-state-ws", {
    transformResponse: function (resp, xhr, elt) {
      // This seems to be a ws response
      if (xhr === null) {
        return JSON.parse(resp).html;
      } else {
        return resp;
      }
    },
    onEvent: function (name, evt) {
      const currentStateKey = parseInt(
        document.getElementById("gameState")?.getAttribute("data-state-key")
      );

      if (name === "htmx:wsConfigSend") {
        evt.detail.parameters.stateKey = currentStateKey;
      }

      if (name === "htmx:wsBeforeMessage") {
        try {
          const { events, html, stateKey, chanMsg } = JSON.parse(
            evt.detail.message
          );
          switch (chanMsg) {
            case "PlayerTyping": {
              if (currentStateKey !== stateKey) {
                evt.preventDefault();
                return;
              }
              break;
            }
            case "AppGameStateChanged": {
              console.log({ stateKey, currentStateKey });
              if (stateKey <= currentStateKey) {
                evt.preventDefault();
                return;
              }
              break;
            }
            default:
              throw new Error(`Unrecognized chanMsg: ${chanMsg}`);
          }
          if (events) {
            for (const event of events) {
              if (event) {
                for (const [key, value] of Object.entries(event)) {
                  htmx.trigger("body", key, value);
                }
              }
            }
          }
        } catch (e) {
          console.error(e);
          evt.preventDefault();
        }
      }
    },
  });

  const tick = new Audio("/static/sounds/Buttons and Navigation/Button 5.m4a");

  const eventSoundDict = {
    WrongGuess: new Audio("/static/sounds/Errors and Cancel/Cancel 1.m4a"),
    CorrectGuess: new Audio(
      "/static/sounds/Complete and Success/Success 2.m4a"
    ),
    MyTurn: new Audio(
      "/static/sounds/Notifications and Alerts/Notification 3.m4a"
    ),
    TimeUp: new Audio("/static/sounds/Errors and Cancel/Error 5.m4a"),
    IWin: new Audio(
      "/static/sounds/Notifications and Alerts/Notification 9.m4a"
    ),
    ILose: new Audio("/static/sounds/Errors and Cancel/Error 4.m4a"),
  };
  for (const event in eventSoundDict) {
    htmx.on(event, () => {
      eventSoundDict[event].play();
    });
  }

  let turnTickingIntervalID;
  htmx.on("MyTurn", () => {
    clearInterval(turnTickingIntervalID);
    turnTickingIntervalID = setInterval(() => {
      tick.play();
    }, 1200);
  });
  htmx.on("TimeUp", () => {
    clearInterval(turnTickingIntervalID);
  });
  htmx.on("CorrectGuess", () => {
    clearInterval(turnTickingIntervalID);
  });
  htmx.on("GameOver", () => {
    clearInterval(turnTickingIntervalID);
  });
})();
