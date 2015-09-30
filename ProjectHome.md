## Vision Statement ##
Provide a high level framework for developing high performance applications that can scale linearly with higher resources.

## Abstract ##
We live in the world where Moore's law and Von Neumann's model of programming will soon be obsolete. As Dr. Dobb's article http://www.ddj.com/web-development/184405990;jsessionid=0LJW3X4MBU5TSQSNDLRSKH0CJUNN2JVN?_requestid=131749 on "Free Lunch is over" indicated we will have to adjust in this world. Fortunately, parallel programming field has been dealing with this for several decades and there are plenty of solutions available. Unfortunately, most of them are still very research oriented and difficult to use in the real world. I wrote similar system called JavaNOW several years ago as a research project in Java that I will port to Erlang. I plan to leverage concurrency capability of Erlang language and its supported libraries such as OTP and provide a framework for writing high performance applications. Though, Erlang is concurrent language and OTP provides sophisticated support for writing fault tolerant services and provides high availability (with hot code swaps), but it does not inherently support writing parallel applications. Erlinda fill in that gap and provide abstraction to write high performance applications. It is built on top of Erlang and OTP and uses RabbitMQ for messaging middleware.


## Introduction ##
Erlinda provides a framework for writing parallel applications. It provides a number of abstractions for creating parallel applications such as:
  * Tuple Space model based on Linda memory model (JavaSpaces)
  * MPI like APIs for communication (Scatter/Gather)
  * Map/Reduce
  * Master/Slave (Computing Farm)
  * Service oriented (OTP)
  * Clustering
  * Agents and Mobililty
  * Messaging oriented (Queues/Topics)
  * Service/Resource discovery mechanism (similar to JINI)
  * Integration with other languages and middlewares
  * Code Server (similar to JINI/RMI)
  * Security

### Project Owner ###
  * Shahzad Bhatti


#### Contact ####
  * Email: bhatti AT plexobject.com
  * URL: http://bhatti.plexobject.com

### Join the Project ###
Though, Erlinda is in early phase of development, anyone who wants to get involved is welcome, especially with experience with Erlang/OTP and parallel computing. Any feedback or suggestions is greatly appreciated.