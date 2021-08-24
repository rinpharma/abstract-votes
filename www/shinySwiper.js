// Shorthand for $( document ).ready()

$(function() {
  //scroll by using the sides.
   var fixed = document.getElementById('swipeCard');
   fixed.addEventListener('touchmove', function(e) {
           e.preventDefault();
   }, false);

  //Grabs the latest paper from the server and displays it to our card.
  Shiny.addCustomMessageHandler("sendingpapers",
    function(data) {
      console.log(data);
      set_card(data.title[0], data.abstract[0],data.byline[0])
    }
  );

  //a function to replace the value of the card
  function set_card(title_text, byline_text, abstract_text){
      var swipeCard = $("#swipeCard");
      var title = $("#cardTitle");
      var byline = $("#cardByline");
      var abstract = $("#cardAbstract");
      title.text(title_text);
      byline.text(byline_text);
      abstract.text(abstract_text);
      //bring the card back to the middle.
      swipeCard.removeClass();
  }

  $("#swipeCard").swipe( {
      //Generic swipe handler for all directions
      swipe: function(event, direction, distance, duration, fingerCount, fingerData) {
        console.log("we got a swipe");
        var swipeCard = $("#swipeCard");
        var decision = null;
        switch (direction) {
              case "up":
                decision = "maybe";
                decision_icon = "<i class = 'fa fa-volume-up fa-5x' aria-hidden='true'></i>";
                swipeCard.addClass("swipe-up");
                break;
              case "down":
                decision = "skipped";
                decision_icon = "<i class = 'fa fa-check fa-5x' aria-hidden='true'></i>"
                swipeCard.addClass("swipe-down");
                break;
              case "right":
                decision = "include";
                decision_icon = "<i class = 'fa fa-star fa-5x' aria-hidden='true'></i>"
                swipeCard.addClass("swipe-right");
                choice = ""
                break;
              case "left":
                decision = "reject";
                decision_icon = "<i class = 'fa fa-trash fa-5x' aria-hidden='true'></i>"
                swipeCard.addClass("swipe-left");
                break;
              default:
                decision = "initializing";
                console.log(direction);
                return; //kill the process so we dont go anywhere.
            }

        //send decision to R.
        Shiny.onInputChange("cardSwiped", decision);


        //wait one second and then reset the card position
        window.setTimeout(() => {

          //reset to deciding so we dont trip up the change detection
          Shiny.onInputChange("cardSwiped", "deciding");

          //bring the card back to the middle.
          //swipeCard.removeClass();

        }
          , 1000);

      } //end of swipe: function(...)

      });


    //wait one second and Kick off stuff by sending an initialized message to R.
    window.setTimeout(() =>  Shiny.onInputChange("cardSwiped", "initializing"), 1000);

   //On mobile when a user tries to swipe up or down they simply get moved around the page. We change//
   //make it such that when they scroll over the card we disable page scrolling. They will still be able


});
