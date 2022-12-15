Twitter clone with websockets

How to set up project and run:
1. Create a project directory and run the following commands:
2. git clone https://github.com/nitrogen/nitrogen.git
3. cd nitrogen 
4. make rel_mochiweb
5. cd ..
6. cd myapp
7. In myapp replace site/src directory with the provided src directory (submission src)
8. Run the following command in the myapp directory:
   bin/nitrogen console
9. website is running on localhost:8000 but has not been recompiled
10. Open new commandline terminal (don't close the nitrogen console terminal) and go to the myapp directory
11. Now go to myapp directory in the new termional. Run : touch site/src/index.erl
12. Run the following command to compile the new src directory we put in:
    bin/dev compile
13. Refresh localhost:8000 should see twitter clone with the options to login or sign up
  
