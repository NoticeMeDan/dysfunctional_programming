# Description of Scrabble project

1. What constraint does your engine have?
   
   At the moment our scrabble bot does only support writing right and down.
   It also only supports extending another word, on either the first or last letter.
   
2. Have you implemented Multiplayer?

   No, only a single bot against itself is supported.
   
3. Have you implemented any parallelism?
    
    Yes, we use parallelism to sum up the points of our pieces.
    It can be found in the `findBestWordForRow` function.

4. Do you handle timeouts?

    No, we do not handle timeouts.