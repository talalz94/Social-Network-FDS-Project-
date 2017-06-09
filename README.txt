# Social-Network-FDS-Project-

Welcome to 'Social Network Analyzer'.

This program is designed to assist in mapping social networks and carry out analysis on them.
The kind of social networks that we are focusing on are inter-university collaborations.
The tools we will be providing in this program will allow the user to find active members in
the graph-space.

The software uses a '.csv' file alongside. Which is contains an Edge at each line in the form
"Name of Student1, Student1 ID, Year of enrollment, Satisfaction, Name of Student1, Student1 ID, Year of enrollment".

Satisfaction is the measure of the Satisfaction of the collaboration between two people.
It was calculated on a scale from 1-5, where

1-wont work again together
2-bad experience
3-neutral 
4-good 
5-would definetly work again

The data for our program was taken from a survey conducted last year in the final project of the social networks course
offered by Dr. Shah Jamal from the group of Sidra Dara and Taimoor Neeshat. The data is stored in the .csv file. However
this graph can be edited as the user desires. 

To edit the graph, update the input1.csv with new nodes and save.



Instructions:

1- Make sure that Social Network.hs and input1.csv are in the same folder.

2- Run Command Prompt and navigate to the folder that contains the two files.

3- In the prompt, type: runhaskell "Social Network.hs".

4- You will be greeted with a Menu screen that will contain 10 functions, represented by the keys 1-10.

5- Follow the instructions in the Menu.

6- The program requires to work with student ID's. For privacy purposes, names and other info are not shown in the program.

7- Incase of error (such as invalid data entry) program will stop. This will require to run the program again.


Functions:

1: Are two people friends 

	Takes two IDs as Input and returns a boolean.
                                                   
2: exist in the list or not    

	Takes an ID and returns a boolean 
                                              
3: connections of a member

	Takes an ID. Outputs a list of all connections(neighbours).
                                                   
4: print all the data   

	Prints all the Student IDs in the list.
                                                     
5: find the mutual friends between two people    

	Takes 2 Student IDs. Returns a list of mutual connections.
                            
6: find the shortest path of a node to all the nodes in the graph 

	Takes a Student ID and outputs a list of shortest paths to all nodes using Dijkstras Algorithm.

	The node is in the form (1392,112,False,[-1])

	Where 1392 is the student ID.
	112 is the distance in terms of edges. (If false, this distance is meaningless)
	False is a boolean representing whether the node was visited or not during traversal. (False if not connected)
	[-1] is a list that shows the next connected node, on the route of shortest path. Where [-1] = The node is the source node. [-2] = Unvisited node.
           
7: To find the most influential person in data  

	Takes a Student ID and calculates the Betweenness centrality. The the node with the highest Betweenness Centrality it outputed. (Might take time depending
        on graph size.)
                            
8: find the Betweeness centrality of a node   

	Takes a Student ID and calculates its Betweenness Centrality.
                               
9: find the shortest distance between two nodes 

	 Outputs a string, thats shows the route from node 'a' to node 'b'.
                            
10: find the most satisfied people till nth term
	Outputs a list of edges containing the n most pairs with highest satisfaction.
	In the form (Student1, Satisfaction, Student2)

Thank You
	

