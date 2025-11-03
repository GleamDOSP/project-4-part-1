# Project 4 – Reddit Clone Engine and Simulator

### Team Members
- **Bakshish Singh** — 8930-8993 — Group 83

---

## 1. Introduction

This project implements the **backend engine** and **client simulator** for a Reddit-like distributed system using the **Gleam programming language**.  
The goal is to simulate large-scale Reddit functionality — including user registration, community creation, posting, commenting, and voting — while analyzing the system’s **scalability**, **throughput**, and **behavior under concurrent load**.

---

## 2. Benchmark Results

| **Users** | **Communities** | **Posts** | **Comments** | **Votes** | **Duration (ms)** | **Throughput (ops/sec)** |
| --------: | --------------: | --------: | -----------: | --------: | ----------------: | -----------------------: |
|     9,000 |              11 |       128 |          747 |     1,263 |             6,192 |                   345.28 |
|    20,000 |              11 |       148 |          947 |     1,651 |            53,563 |                   249.40 |
|    30,000 |              11 |       233 |        1,019 |     1,460 |           161,179 |                   114.96 |

---

## 3. Performance Analysis

* **Scalability:** The system efficiently scales to tens of thousands of users while maintaining stable operations and balanced workloads.
* **Throughput Behavior:** Throughput decreases with higher user counts due to increased message routing and scheduling overhead, which is typical in distributed actor-based simulations.
* **Zipf Distribution Impact:** Popular communities produce more posts and interactions, accurately mirroring real-world engagement dynamics.
* **Concurrency Model:** Each user runs as an independent Gleam process, achieving parallel execution and realistic workload simulation.

---
## 4. Implementation Details

### Language and Environment
- **Language:** Gleam  
- **Execution:** `gleam build` and `gleam run`  
- **Platform:** macOS (tested on Apple Silicon and Intel)  
- **Concurrency:** Implemented using Gleam processes and asynchronous message passing  

---

### Project Structure

```

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

````

---

### How to Modify Simulation Parameters

Simulation parameters can be adjusted in  
`src/simulation/main.gleam` under the following section:

```gleam
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
````

---

## 5. Features

### Core Functionality

* ✅ User registration and management
* ✅ Subreddit creation, joining, and leaving
* ✅ Post creation and hierarchical commenting
* ✅ Upvote/downvote system with karma computation
* ✅ Feed generation from subscribed subreddits
* ✅ Direct messaging with reply support
* ✅ Repost functionality

---

### Architecture

* **Actor Model:** A single engine process coordinates all operations through Gleam’s actor-based concurrency.
* **Message Passing:** Components communicate using asynchronous message passing to ensure non-blocking execution.
* **Separation of Concerns:** Engine and client simulators operate as distinct processes, simplifying logic and isolation.

---

### Simulator Features

* ✅ Configurable number of simulated users
* ✅ Connection/disconnection simulation (users periodically disconnect and reconnect)
* ✅ Zipf distribution for subreddit membership (popular communities have more members)
* ✅ Zipf-based post generation (popular communities receive more posts)
* ✅ Performance metrics collection for latency and throughput
* ✅ Randomized user activities (feed fetching, posting, voting, messaging, etc.)

---

## 6. Engine Actor

The **engine** operates as a **single actor process** responsible for maintaining the complete system state.
It manages:

* User registry
* Subreddit registry
* Posts and comments
* Direct messages
* Karma tracking

All operations are processed through **message passing**.
The engine receives messages of type `EngineMessage` and responds with corresponding `EngineResponse` messages.
This design ensures **full concurrency** and **state isolation**, consistent with actor-based distributed principles.

---

## 7. Zipf Distribution

The simulator uses a **Zipf distribution** to model realistic Reddit-like content and user engagement patterns:

* **Subreddit Membership:** Popular subreddits (lower rank) attract exponentially more members.
* **Post Distribution:** Highly ranked communities receive more frequent posts.
* **Mathematical Model:**
  [
  P(\text{rank}) = \frac{1}{\text{rank}^s}
  ]
  where `s` is the Zipf parameter controlling skewness (e.g., `s = 1.0` for moderate bias).

This approach ensures that engagement naturally concentrates in a few popular subreddits, reflecting real-world dynamics.

---

## 8. Connection Simulation

The **connection simulator** models realistic client connectivity patterns with alternating online/offline cycles:

* Users connect and disconnect periodically during the simulation.
* **Average connection time:** approximately **7 seconds**
* **Average disconnection time:** approximately **4 seconds**
* User operations (posting, commenting, voting) occur **only while connected**, preserving authenticity.

This behavior closely replicates dynamic user participation and network fluctuations found in large-scale social platforms.

---

## 9. How to Run the Project

1. Extract the archive:

   ```bash
   unzip project4.zip
   ```

2. Navigate to the project directory:

   ```bash
   cd RedditEngine
   ```

3. Build the project:

   ```bash
   gleam build
   ```

4. Run the simulation:

   ```bash
   gleam run
   ```

---



## 10. Conclusion

The **Reddit Clone Engine** successfully demonstrates a distributed simulation capable of modeling Reddit’s core behaviors at scale, including:

* User and community management
* Post and comment operations
* Voting and karma computation
* Performance measurement under varying load

This implementation establishes a robust foundation for **Part II**, where REST APIs or WebSocket interfaces can be added for real-time interactivity and external client integration.

---

**Author:** Bakshish Singh
**Course Project:** Distributed Systems — Project 4
**Language:** Gleam

```
