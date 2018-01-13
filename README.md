# thorough-search

Generates phrases with a given structure by recursively expanding based on results from Google's autocomplete API. Expansion happens whenever the API returns 10 results, since this is the max and implies that there are more to find. 

During the search phase, all results are recorded in a database, from which they're retrieved after the search phase complete. They are then filtered using SCOWL word lists, and outputted to a text file which contains the entire filtered result list sorted and grouped by commonality.

## TODO
 - automatically email results