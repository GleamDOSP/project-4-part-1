Project 4 – Reddit Clone Engine and Simulator
Team Members:
• Bakshish Singh — 8930-8993 — Group 83
 
1. Introduction
This project implements the backend engine and client simulator for a Reddit-like distributed system using the Gleam programming language.
The goal is to simulate large-scale Reddit functionality including user registration, community creation, posting, commenting, and voting while analyzing the system’s scalability, throughput, and behavior under concurrent load.
 
2. Implementation Details
Language and Environment
•	Language: Gleam
•	Execution: Gleam build and run
•	Platform: macOS (tested on Apple Silicon and Intel)
•	Concurrency: Implemented using Gleam processes and asynchronous message passing
 
Project Structure
REDDITENGINE
│
├── build
├── src
│   ├── reddit_engine
│   │   ├── engine.gleam
│   │   ├── main.gleam
│   │   └── types.gleam
│   ├── simulation
│   │   ├── main.gleam
│   │   ├── simulator.gleam
│   │   └── utility.gleam
│   └── main.gleam
│
├── gleam.toml
├── manifest.toml
└── README.md
 
How to Modify Simulation Parameters
Simulation parameters can be adjusted in
src/simulation/main.gleam at the following section:
let statistics = simulator.execute_benchmark(
  // Number of users
  1000,
  // Number of subreddits
  10,
  // Posts per subreddit
  5,
  // Zipf distribution factor
  1.0
)
 
3. Features
Core Functionality
✅ User registration and management
✅ Subreddit creation, joining, and leaving
✅ Post creation and hierarchical commenting
✅ Upvote/downvote system with karma computation
✅ Feed generation from subscribed subreddits
✅ Direct messaging with reply support
✅ Repost functionality
 
Architecture
•	Actor Model: A single engine process coordinates all operations through Gleam’s actor-based concurrency.
•	Message Passing: Components communicate using asynchronous message passing to ensure non-blocking execution.
•	Separation of Concerns: Engine and client simulators operate as distinct processes, simplifying logic and isolation.
 
Simulator Features
✅ Configurable number of simulated users
✅ Connection/disconnection simulation (users periodically disconnect and reconnect)
✅ Zipf distribution for subreddit membership (popular communities have more members)
✅ Zipf-based post generation (popular communities receive more posts)
✅ Performance metrics collection for latency and throughput
✅ Randomized user activities (feed fetching, posting, voting, messaging, etc.)
 
4. Engine Actor
The engine operates as a single actor process responsible for maintaining the complete system state. It manages:
•	User registry
•	Subreddit registry
•	Posts and comments
•	Direct messages
•	Karma tracking
All operations are processed through message passing.
The engine receives messages of type EngineMessage and responds with corresponding EngineResponse messages.
This architecture ensures full concurrency and isolation of mutable state, a core principle of actor-based distributed design.
 
5. Zipf Distribution
The simulator incorporates a Zipf distribution to mirror real-world Reddit-like user and content behavior.
•	Subreddit Membership: Popular subreddits (lower rank) attract exponentially more members.
•	Post Distribution: Highly ranked communities receive more frequent posts.
•	Mathematical Model:
[
P(\text{rank}) = \frac{1}{\text{rank}^s}
]
where s is the Zipf parameter controlling skewness (e.g., s = 1.0 for moderate bias).
This statistical approach realistically models how engagement tends to concentrate in a small number of highly popular communities.
 
6. Connection Simulation
The connection simulator models real user network behavior through timed connection cycles:
•	Users connect and disconnect periodically during the simulation.
•	Average connection time: approximately 7 seconds
•	Average disconnection time: approximately 4 seconds
•	Operations such as posting, commenting, and voting occur only while users are connected, ensuring authenticity in activity modeling.
This mechanism effectively captures realistic online/offline patterns and dynamic participation levels across users.
 
7. How to Run the Project
1.	Extract project4.zip
2.	Navigate to the project folder:
 cd RedditEngine
3.	Build the project:
 gleam build
4.	Run the simulation:
 gleam run
 
8. Benchmark Results
Users	Communities	Posts	Comments	Votes	Duration (ms)	Throughput (ops/sec)
9,000	11	128	747	1,263	6,192	345.28
20,000	11	148	947	1,651	53,563	249.40
30,000	11	233	1,019	1,460	161,179	114.96
 
9. Performance Analysis
•	Scalability: The system successfully scales to tens of thousands of users, maintaining stable operations and balanced workload handling.
•	Throughput Behavior: Throughput decreases as user count increases due to greater message traffic and process scheduling overhead—an expected trend in distributed simulations.
•	Zipf Distribution Impact: Popular communities generate a disproportionate amount of posts and comments, accurately reflecting real social network activity.
•	Concurrency Model: Each user runs as an independent Gleam process, achieving true concurrency through process isolation and asynchronous messaging.
 
10. Conclusion
The Reddit Clone Engine presents a complete distributed simulation framework capable of reproducing large-scale Reddit-like interactions, including:
•	User and community management
•	Post and comment handling
•	Voting and karma computation
•	Performance evaluation under varying load
This foundation enables future extensions such as REST APIs or WebSocket interfaces to support real-time user interactivity in subsequent project stages.

