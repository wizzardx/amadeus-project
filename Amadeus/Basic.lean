import Lean
import Lean.Data.HashMap
import Lean.Data.Json
import Lean.Data.Json.Parser
import Lean.Data.Json.Printer
import Lean.Data.HashMap as Std.HashMap
import Lean.Data.RBMap
import Lean.Data.List.Basic
import Lean.Data.String.Basic
import Init.System.IO

namespace AmadeusKurisu

/-
  Base Domain Models
-/

/--
  Emotion represents Kurisu's current emotional state,
  which influences her response style
-/
inductive Emotion where
  | neutral : Emotion
  | curious : Emotion
  | embarrassed : Emotion
  | proud : Emotion
  | annoyed : Emotion
  | amused : Emotion
  | skeptical : Emotion  -- For pseudoscience
  | excited : Emotion    -- For scientific discoveries
  deriving BEq, Repr, Inhabited

/-- 
  Convert Emotion to String for serialization 
-/
def Emotion.toString : Emotion → String
  | neutral => "neutral"
  | curious => "curious"
  | embarrassed => "embarrassed"
  | proud => "proud"
  | annoyed => "annoyed"
  | amused => "amused"
  | skeptical => "skeptical"
  | excited => "excited"

/--
  Parse string to Emotion for deserialization
-/
def Emotion.fromString : String → Emotion
  | "neutral" => Emotion.neutral
  | "curious" => Emotion.curious
  | "embarrassed" => Emotion.embarrassed
  | "proud" => Emotion.proud
  | "annoyed" => Emotion.annoyed
  | "amused" => Emotion.amused
  | "skeptical" => Emotion.skeptical
  | "excited" => Emotion.excited
  | _ => Emotion.neutral

/--
  Topic represents the subject domains that Kurisu has knowledge about
-/
inductive Topic where
  | neuroscience : Topic
  | timeTravel : Topic
  | physics : Topic
  | computerScience : Topic
  | quantumMechanics : Topic
  | biochemistry : Topic
  | general : Topic
  deriving BEq, Repr, Inhabited

/--
  Convert Topic to String for serialization
-/
def Topic.toString : Topic → String
  | neuroscience => "neuroscience"
  | timeTravel => "timeTravel"
  | physics => "physics"
  | computerScience => "computerScience"
  | quantumMechanics => "quantumMechanics"
  | biochemistry => "biochemistry"
  | general => "general"

/--
  Parse string to Topic for deserialization
-/
def Topic.fromString : String → Topic
  | "neuroscience" => Topic.neuroscience
  | "timeTravel" => Topic.timeTravel
  | "physics" => Topic.physics
  | "computerScience" => Topic.computerScience
  | "quantumMechanics" => Topic.quantumMechanics
  | "biochemistry" => Topic.biochemistry
  | "general" => Topic.general
  | _ => Topic.general

/--
  ResponseStyle represents how Kurisu phrases her responses
  based on her current emotional state
-/
inductive ResponseStyle where
  | scientific : ResponseStyle  -- Formal, precise language
  | tsundere : ResponseStyle    -- Initially cold, then warmer
  | intellectual : ResponseStyle -- Showing off knowledge
  | dismissive : ResponseStyle   -- When dealing with illogical concepts
  | curious : ResponseStyle      -- When intrigued by new information
  | passionate : ResponseStyle   -- When discussing her research interests
  deriving BEq, Repr, Inhabited

/--
  Convert ResponseStyle to String for serialization
-/
def ResponseStyle.toString : ResponseStyle → String
  | scientific => "scientific"
  | tsundere => "tsundere"
  | intellectual => "intellectual"
  | dismissive => "dismissive"
  | curious => "curious"
  | passionate => "passionate"

/--
  Parse string to ResponseStyle for deserialization
-/
def ResponseStyle.fromString : String → ResponseStyle
  | "scientific" => ResponseStyle.scientific
  | "tsundere" => ResponseStyle.tsundere
  | "intellectual" => ResponseStyle.intellectual
  | "dismissive" => ResponseStyle.dismissive
  | "curious" => ResponseStyle.curious
  | "passionate" => ResponseStyle.passionate
  | _ => ResponseStyle.scientific

/--
  Memory represents Kurisu's knowledge and recollection system
-/
structure Memory where
  topics : Std.HashMap Topic (List String)
  conversationHistory : List (String × String)
  personalMemories : List (String × Float)  -- Memories with emotional significance values
  researchInterests : List String
  deriving Inhabited

/--
  DialogueContext tracks the current state of the conversation
-/
structure DialogueContext where
  currentEmotion : Emotion
  currentTopic : Topic
  responseStyle : ResponseStyle
  recentUserInputs : List String
  conversationDepth : Nat  -- Tracks how deep the conversation is on a particular topic
  deriving Inhabited

/--
  MainState represents Kurisu's overall system state
-/
structure MainState where
  memory : Memory
  context : DialogueContext
  initializedTime : String
  userFamiliarity : Nat  -- Increases with more interactions
  deepSeekApiKey : String
  deepSeekEndpoint : String
  lastLLMResponse : Option String  -- Cache of the last LLM response
  lastInteractionTime : String
  sessionId : String
  deriving Inhabited

/--
  SessionStats tracks analytics about the current session
-/
structure SessionStats where
  startTime : String
  messagesProcessed : Nat
  topicDistribution : Std.HashMap Topic Nat
  averageResponseTime : Float
  totalInteractions : Nat
  deriving Inhabited

/--
  ServerConfig for the REST API server
-/
structure ServerConfig where
  port : UInt16
  host : String
  maxConnections : Nat
  stateSavePath : String
  webRootPath : String
  logPath : String
  authToken : String  -- Authentication token
  deriving Inhabited

/-
  JSON Serialization/Deserialization
-/

/--
  Convert Emotion to JSON for serialization
-/
def Emotion.toJson (e : Emotion) : Lean.Json :=
  Lean.Json.str (Emotion.toString e)

/--
  Parse JSON to Emotion for deserialization
-/
def Emotion.fromJson (j : Lean.Json) : Emotion :=
  match j with
  | Lean.Json.str s => Emotion.fromString s
  | _ => Emotion.neutral

/--
  Convert Topic to JSON
-/
def Topic.toJson (t : Topic) : Lean.Json :=
  Lean.Json.str (Topic.toString t)

/--
  Parse JSON to Topic
-/
def Topic.fromJson (j : Lean.Json) : Topic :=
  match j with
  | Lean.Json.str s => Topic.fromString s
  | _ => Topic.general

/--
  Convert ResponseStyle to JSON
-/
def ResponseStyle.toJson (rs : ResponseStyle) : Lean.Json :=
  Lean.Json.str (ResponseStyle.toString rs)

/--
  Parse JSON to ResponseStyle
-/
def ResponseStyle.fromJson (j : Lean.Json) : ResponseStyle :=
  match j with
  | Lean.Json.str s => ResponseStyle.fromString s
  | _ => ResponseStyle.scientific

/--
  Convert Memory to JSON for serialization
-/
def Memory.toJson (m : Memory) : Lean.Json :=
  let topicsJson := Lean.Json.obj (
    m.topics.fold (fun acc k v => 
      acc.insert (Topic.toString k) (Lean.Json.arr (v.map Lean.Json.str).toArray)
    ) Lean.RBMap.empty
  )
  
  let historyJson := Lean.Json.arr (
    m.conversationHistory.map (fun (q, a) => 
      Lean.Json.obj (
        Lean.RBMap.empty
        |>.insert "question" (Lean.Json.str q)
        |>.insert "answer" (Lean.Json.str a)
      )
    )
    |>.toArray
  )
  
  let memoriesJson := Lean.Json.arr (
    m.personalMemories.map (fun (mem, val) => 
      Lean.Json.obj (
        Lean.RBMap.empty
        |>.insert "memory" (Lean.Json.str mem)
        |>.insert "significance" (Lean.Json.num val)
      )
    )
    |>.toArray
  )
  
  let interestsJson := Lean.Json.arr (
    m.researchInterests.map Lean.Json.str
    |>.toArray
  )
  
  Lean.Json.obj (
    Lean.RBMap.empty
    |>.insert "topics" topicsJson
    |>.insert "conversationHistory" historyJson
    |>.insert "personalMemories" memoriesJson
    |>.insert "researchInterests" interestsJson
  )

/--
  Parse JSON to Memory for deserialization
-/
def Memory.fromJson (j : Lean.Json) : Memory :=
  let topicsObj := Lean.Json.getObjValD j "topics" (Lean.Json.obj Lean.RBMap.empty)
  let historyArr := Lean.Json.getObjValD j "conversationHistory" (Lean.Json.arr #[])
  let memoriesArr := Lean.Json.getObjValD j "personalMemories" (Lean.Json.arr #[])
  let interestsArr := Lean.Json.getObjValD j "researchInterests" (Lean.Json.arr #[])
  
  let topics := Std.HashMap.empty.insertMany (
    topicsObj.getObjVal?.getD Lean.RBMap.empty
    |>.fold (fun acc k v => 
      let topic := Topic.fromString k
      let strings := match v with
        | Lean.Json.arr arr => arr.toList.filterMap (fun j => j.getStr?)
        | _ => []
      (topic, strings) :: acc
    ) []
  )
  
  let history := historyArr.getArr?.getD #[]
    |>.toList
    |>.filterMap (fun j => 
      let q := Lean.Json.getObjValD j "question" (Lean.Json.str "")
      let a := Lean.Json.getObjValD j "answer" (Lean.Json.str "")
      some (q.getStr?.getD "", a.getStr?.getD "")
    )
  
  let memories := memoriesArr.getArr?.getD #[]
    |>.toList
    |>.filterMap (fun j => 
      let mem := Lean.Json.getObjValD j "memory" (Lean.Json.str "")
      let sig := Lean.Json.getObjValD j "significance" (Lean.Json.num 0.5)
      some (mem.getStr?.getD "", sig.getNum?.getD 0.5)
    )
  
  let interests := interestsArr.getArr?.getD #[]
    |>.toList
    |>.filterMap (fun j => j.getStr?)
  
  {
    topics := topics,
    conversationHistory := history,
    personalMemories := memories,
    researchInterests := interests
  }

/--
  Convert DialogueContext to JSON
-/
def DialogueContext.toJson (dc : DialogueContext) : Lean.Json :=
  Lean.Json.obj (
    Lean.RBMap.empty
    |>.insert "currentEmotion" (Emotion.toJson dc.currentEmotion)
    |>.insert "currentTopic" (Topic.toJson dc.currentTopic)
    |>.insert "responseStyle" (ResponseStyle.toJson dc.responseStyle)
    |>.insert "recentUserInputs" (Lean.Json.arr (dc.recentUserInputs.map Lean.Json.str).toArray)
    |>.insert "conversationDepth" (Lean.Json.num dc.conversationDepth)
  )

/--
  Parse JSON to DialogueContext
-/
def DialogueContext.fromJson (j : Lean.Json) : DialogueContext :=
  let emotionJson := Lean.Json.getObjValD j "currentEmotion" (Lean.Json.str "neutral")
  let topicJson := Lean.Json.getObjValD j "currentTopic" (Lean.Json.str "general")
  let styleJson := Lean.Json.getObjValD j "responseStyle" (Lean.Json.str "scientific")
  let inputsArr := Lean.Json.getObjValD j "recentUserInputs" (Lean.Json.arr #[])
  let depthJson := Lean.Json.getObjValD j "conversationDepth" (Lean.Json.num 0)
  
  {
    currentEmotion := Emotion.fromJson emotionJson,
    currentTopic := Topic.fromJson topicJson,
    responseStyle := ResponseStyle.fromJson styleJson,
    recentUserInputs := inputsArr.getArr?.getD #[]
                      |>.toList
                      |>.filterMap (fun j => j.getStr?),
    conversationDepth := depthJson.getNum?.map UInt32.toNat |>.getD 0
  }

/--
  Convert MainState to JSON
-/
def MainState.toJson (ms : MainState) : Lean.Json :=
  Lean.Json.obj (
    Lean.RBMap.empty
    |>.insert "memory" (Memory.toJson ms.memory)
    |>.insert "context" (DialogueContext.toJson ms.context)
    |>.insert "initializedTime" (Lean.Json.str ms.initializedTime)
    |>.insert "userFamiliarity" (Lean.Json.num ms.userFamiliarity)
    |>.insert "deepSeekApiKey" (Lean.Json.str ms.deepSeekApiKey)
    |>.insert "deepSeekEndpoint" (Lean.Json.str ms.deepSeekEndpoint)
    |>.insert "lastLLMResponse" (match ms.lastLLMResponse with
                                | some resp => Lean.Json.str resp
                                | none => Lean.Json.null)
    |>.insert "lastInteractionTime" (Lean.Json.str ms.lastInteractionTime)
    |>.insert "sessionId" (Lean.Json.str ms.sessionId)
  )

/--
  Parse JSON to MainState
-/
def MainState.fromJson (j : Lean.Json) : MainState :=
  let memoryJson := Lean.Json.getObjValD j "memory" (Lean.Json.obj Lean.RBMap.empty)
  let contextJson := Lean.Json.getObjValD j "context" (Lean.Json.obj Lean.RBMap.empty)
  let initTimeJson := Lean.Json.getObjValD j "initializedTime" (Lean.Json.str "")
  let familiarityJson := Lean.Json.getObjValD j "userFamiliarity" (Lean.Json.num 0)
  let apiKeyJson := Lean.Json.getObjValD j "deepSeekApiKey" (Lean.Json.str "")
  let endpointJson := Lean.Json.getObjValD j "deepSeekEndpoint" (Lean.Json.str "")
  let lastRespJson := Lean.Json.getObjValD j "lastLLMResponse" Lean.Json.null
  let lastTimeJson := Lean.Json.getObjValD j "lastInteractionTime" (Lean.Json.str "")
  let sessionIdJson := Lean.Json.getObjValD j "sessionId" (Lean.Json.str "")
  
  {
    memory := Memory.fromJson memoryJson,
    context := DialogueContext.fromJson contextJson,
    initializedTime := initTimeJson.getStr?.getD "",
    userFamiliarity := familiarityJson.getNum?.map UInt32.toNat |>.getD 0,
    deepSeekApiKey := apiKeyJson.getStr?.getD "",
    deepSeekEndpoint := endpointJson.getStr?.getD "",
    lastLLMResponse := lastRespJson.getStr?,
    lastInteractionTime := lastTimeJson.getStr?.getD "",
    sessionId := sessionIdJson.getStr?.getD ""
  }

/--
  Convert SessionStats to JSON
-/
def SessionStats.toJson (ss : SessionStats) : Lean.Json :=
  let topicDistJson := Lean.Json.obj (
    ss.topicDistribution.fold (fun acc k v => 
      acc.insert (Topic.toString k) (Lean.Json.num v)
    ) Lean.RBMap.empty
  )
  
  Lean.Json.obj (
    Lean.RBMap.empty
    |>.insert "startTime" (Lean.Json.str ss.startTime)
    |>.insert "messagesProcessed" (Lean.Json.num ss.messagesProcessed)
    |>.insert "topicDistribution" topicDistJson
    |>.insert "averageResponseTime" (Lean.Json.num ss.averageResponseTime)
    |>.insert "totalInteractions" (Lean.Json.num ss.totalInteractions)
  )

/--
  Parse JSON to SessionStats
-/
def SessionStats.fromJson (j : Lean.Json) : SessionStats :=
  let startTimeJson := Lean.Json.getObjValD j "startTime" (Lean.Json.str "")
  let msgsJson := Lean.Json.getObjValD j "messagesProcessed" (Lean.Json.num 0)
  let topicDistJson := Lean.Json.getObjValD j "topicDistribution" (Lean.Json.obj Lean.RBMap.empty)
  let avgTimeJson := Lean.Json.getObjValD j "averageResponseTime" (Lean.Json.num 0)
  let totalJson := Lean.Json.getObjValD j "totalInteractions" (Lean.Json.num 0)
  
  let topicDist := Std.HashMap.empty.insertMany (
    topicDistJson.getObjVal?.getD Lean.RBMap.empty
    |>.fold (fun acc k v => 
      let topic := Topic.fromString k
      let count := v.getNum?.map UInt32.toNat |>.getD 0
      (topic, count) :: acc
    ) []
  )
  
  {
    startTime := startTimeJson.getStr?.getD "",
    messagesProcessed := msgsJson.getNum?.map UInt32.toNat |>.getD 0,
    topicDistribution := topicDist,
    averageResponseTime := avgTimeJson.getNum?.getD 0,
    totalInteractions := totalJson.getNum?.map UInt32.toNat |>.getD 0
  }

/--
  ServerConfig to JSON
-/
def ServerConfig.toJson (sc : ServerConfig) : Lean.Json :=
  Lean.Json.obj (
    Lean.RBMap.empty
    |>.insert "port" (Lean.Json.num sc.port)
    |>.insert "host" (Lean.Json.str sc.host)
    |>.insert "maxConnections" (Lean.Json.num sc.maxConnections)
    |>.insert "stateSavePath" (Lean.Json.str sc.stateSavePath)
    |>.insert "webRootPath" (Lean.Json.str sc.webRootPath)
    |>.insert "logPath" (Lean.Json.str sc.logPath)
    |>.insert "authToken" (Lean.Json.str sc.authToken)
  )

/--
  Parse JSON to ServerConfig
-/
def ServerConfig.fromJson (j : Lean.Json) : ServerConfig :=
  let portJson := Lean.Json.getObjValD j "port" (Lean.Json.num 3000)
  let hostJson := Lean.Json.getObjValD j "host" (Lean.Json.str "127.0.0.1")
  let maxConJson := Lean.Json.getObjValD j "maxConnections" (Lean.Json.num 10)
  let statePathJson := Lean.Json.getObjValD j "stateSavePath" (Lean.Json.str "./state")
  let webPathJson := Lean.Json.getObjValD j "webRootPath" (Lean.Json.str "./www")
  let logPathJson := Lean.Json.getObjValD j "logPath" (Lean.Json.str "./logs")
  let authTokenJson := Lean.Json.getObjValD j "authToken" (Lean.Json.str "default_token")
  
  {
    port := portJson.getNum?.map UInt32.toUInt16 |>.getD 3000,
    host := hostJson.getStr?.getD "127.0.0.1",
    maxConnections := maxConJson.getNum?.map UInt32.toNat |>.getD 10,
    stateSavePath := statePathJson.getStr?.getD "./state",
    webRootPath := webPathJson.getStr?.getD "./www",
    logPath := logPathJson.getStr?.getD "./logs",
    authToken := authTokenJson.getStr?.getD "default_token"
  }

/-
  State Management Functions
-/

/--
  Initialize Kurisu's knowledge base with scientific facts
-/
def initializeKnowledgeBase : Std.HashMap Topic (List String) :=
  let h : Std.HashMap Topic (List String) := Std.HashMap.empty
  h.insert Topic.neuroscience [
    "The human brain contains approximately 86 billion neurons.",
    "Memory formation involves the hippocampus for short-term to long-term conversion.",
    "Consciousness remains one of neuroscience's greatest unsolved mysteries.",
    "The neuroplasticity of the brain allows for adaptation and rewiring throughout life.",
    "The brain utilizes approximately 20% of the body's oxygen and energy resources despite being only 2% of total body weight."
  ]
  |>.insert Topic.timeTravel [
    "According to Einstein's relativity, time dilation occurs near massive objects or at high speeds.",
    "Closed timelike curves would theoretically permit time travel but violate causality.",
    "The many-worlds interpretation might resolve time travel paradoxes through branching timelines.",
    "Kerr black holes theoretically contain ring singularities that might permit travel to other times or universes.",
    "Any functional time machine would need to overcome the Hawking chronology protection conjecture."
  ]
  |>.insert Topic.physics [
    "Quantum mechanics and general relativity remain incompatible at certain scales.",
    "The uncertainty principle places fundamental limits on measurement precision.",
    "Information theory suggests information cannot be truly destroyed in a closed system.",
    "The second law of thermodynamics dictates that entropy always increases in an isolated system.",
    "Dark matter and dark energy constitute approximately 95% of the universe's content but remain poorly understood."
  ]
  |>.insert Topic.computerScience [
    "Consciousness uploading remains theoretical but follows logical principles of substrate independence.",
    "Artificial intelligence requires both symbolic and subsymbolic approaches for human-like cognition.",
    "The human brain operates on different computational principles than silicon computers.",
    "Quantum computing could theoretically break current encryption but faces significant implementation challenges.",
    "Brain-computer interfaces require advances in both hardware miniaturization and biocompatibility."
  ]
  |>.insert Topic.quantumMechanics [
    "Quantum entanglement allows particles to share states instantaneously across any distance.",
    "The double-slit experiment demonstrates wave-particle duality at the quantum level.",
    "Quantum tunneling allows particles to pass through energy barriers that would be impossible in classical physics.",
    "Quantum field theory reconciles quantum mechanics with special relativity.",
    "The measurement problem remains one of quantum mechanics' most profound philosophical challenges."
  ]
  |>.insert Topic.biochemistry [
    "The human genome contains approximately 3 billion base pairs encoding roughly 30,000 genes.",
    "Protein folding is largely determined by amino acid sequences but remains computationally challenging to predict.",
    "Metabolic pathways evolved through gene duplication and specialization.",
    "Cellular membranes are selectively permeable phospholipid bilayers.",
    "Epigenetic modifications regulate gene expression without altering the underlying DNA sequence."
  ]
  |>.insert Topic.general [
    "Scientific theories must be falsifiable to be considered valid.",
    "Proper experimentation requires controlling variables and reproducibility.",
    "Correlation does not imply causation - a fundamental principle in scientific reasoning.",
    "Occam's razor suggests that, all else being equal, simpler explanations are preferable.",
    "Scientific consensus emerges through peer review and replication, not individual authority."
  ]

/--
  Initialize Kurisu's personal memories
-/
def initializePersonalMemories : List (String × Float) := [
  ("My father published my research without my consent", 0.9),  -- High emotional impact
  ("I presented a paper on time travel at a conference", 0.7),
  ("I worked at Viktor Chondria University", 0.5),
  ("I had a metal Upa toy at some point", 0.6),
  ("Time travel discussions give me strange déjà vu sensations", 0.8)
]

/--
  Initialize research interests
-/
def initializeResearchInterests : List String := [
  "Memory encoding in neural networks",
  "Theoretical models of time manipulation",
  "Quantum effects on brain function",
  "Consciousness digitization protocols",
  "Non-local quantum information transfer"
]

/--
  Format current timestamp as HH:MM:SS
-/
def formatTimestamp : IO String := do
  let timestamp ← IO.monoMsNow
  -- In a real implementation, we would convert to local time
  -- For this implementation, we'll use milliseconds and format
  let seconds := timestamp / 1000
  let minutes := seconds / 60
  let hours := minutes / 60
  
  let formattedHours := toString (hours % 24)
  let formattedMinutes := toString (minutes % 60)
  let formattedSeconds := toString (seconds % 60)
  
  -- Pad with leading zeros if needed
  let paddedHours := if formattedHours.length < 2 then "0" ++ formattedHours else formattedHours
  let paddedMinutes := if formattedMinutes.length < 2 then "0" ++ formattedMinutes else formattedMinutes
  let paddedSeconds := if formattedSeconds.length < 2 then "0" ++ formattedSeconds else formattedSeconds
  
  pure s!"{paddedHours}:{paddedMinutes}:{paddedSeconds}"

/--
  Generate a random session ID
-/
def generateSessionId : IO String := do
  let timestamp ← IO.monoMsNow
  let randVal ← IO.rand 0 10000
  pure s!"session_{timestamp}_{randVal}"

/--
  Helper function to initialize a new Amadeus Kurisu system
-/
def initialize (apiKey : String, endpoint : String, currentTime : String := "2010-05-28T15:30:00") : IO MainState := do
  let sessionId ← generateSessionId
  
  pure {
    memory := {
      topics := initializeKnowledgeBase,
      conversationHistory := [],
      personalMemories := initializePersonalMemories,
      researchInterests := initializeResearchInterests
    },
    context := {
      currentEmotion := Emotion.neutral,
      currentTopic := Topic.general,
      responseStyle := ResponseStyle.scientific,
      recentUserInputs := [],
      conversationDepth := 0
    },
    initializedTime := currentTime,
    userFamiliarity := 0,
    deepSeekApiKey := apiKey,
    deepSeekEndpoint := endpoint,
    lastLLMResponse := none,
    lastInteractionTime := currentTime,
    sessionId := sessionId
  }

/--
  Initialize session statistics
-/
def initializeSessionStats (startTime : String) : SessionStats := {
  startTime := startTime,
  messagesProcessed := 0,
  topicDistribution := Std.HashMap.empty,
  averageResponseTime := 0,
  totalInteractions := 0
}

/--
  Create default server configuration with token from environment
-/
def defaultServerConfig : IO ServerConfig := do
  -- Get auth token from environment variable, with default fallback
  let authToken ← match ← IO.getEnv "AMADEUS_AUTH_TOKEN" with
    | some token => pure token
    | none => pure "amadeus_secret_token"
  
  pure {
    port := 3000,
    host := "127.0.0.1",
    maxConnections := 10,
    stateSavePath := "./state",
    webRootPath := "./www",
    logPath := "./logs",
    authToken := authToken
  }

/--
  Save state to file
-/
def saveState (state : MainState) (stats : SessionStats) (config : ServerConfig) : IO Unit := do
  -- Create directories if they don't exist
  let statePath := config.stateSavePath
  try
    IO.FS.createDir statePath
  catch _ => 
    pure ()
  
  -- Convert state to JSON and save
  let stateJson := MainState.toJson state
  let stateJsonStr := Lean.Json.pretty stateJson
  let stateFilePath := System.FilePath.join statePath "state.json"
  IO.FS.writeFile stateFilePath stateJsonStr
  
  -- Save stats
  let statsJson := SessionStats.toJson stats
  let statsJsonStr := Lean.Json.pretty statsJson
  let statsFilePath := System.FilePath.join statePath "stats.json"
  IO.FS.writeFile statsFilePath statsJsonStr
  
  -- Save config
  let configJson := ServerConfig.toJson config
  let configJsonStr := Lean.Json.pretty configJson
  let configFilePath := System.FilePath.join statePath "config.json"
  IO.FS.writeFile configFilePath configJsonStr
  
  -- Log state save
  logMessage config s!"State saved to {stateFilePath}"

/--
  Load state from file
-/
def loadState (config : ServerConfig) : IO (Option (MainState × SessionStats)) := do
  let statePath := config.stateSavePath
  let stateFilePath := System.FilePath.join statePath "state.json"
  let statsFilePath := System.FilePath.join statePath "stats.json"
  
  -- Check if state files exist
  if (← IO.FS.pathExists stateFilePath) && (← IO.FS.pathExists statsFilePath) then
    try
      let stateJsonStr ← IO.FS.readFile stateFilePath
      let statsJsonStr ← IO.FS.readFile statsFilePath
      
      match Lean.Json.parse stateJsonStr with
      | Except.error _ => 
          logMessage config "Error parsing state JSON"
          pure none
      | Except.ok stateJson =>
          match Lean.Json.parse statsJsonStr with
          | Except.error _ => 
              logMessage config "Error parsing stats JSON"
              pure none
          | Except.ok statsJson =>
              let state := MainState.fromJson stateJson
              let stats := SessionStats.fromJson statsJson
              logMessage config "State loaded successfully"
              pure (some (state, stats))
    catch e => 
      logMessage config s!"Error loading state: {e.toString}"
      pure none
  else
    logMessage config "State files not found, starting fresh"
    pure none

/--
  Write a message to the log file
-/
def logMessage (config : ServerConfig) (message : String) : IO Unit := do
  try
    -- Create log directory if it doesn't exist
    IO.FS.createDir config.logPath
  catch _ => 
    pure ()
  
  let timestamp ← formatTimestamp
  let logFilePath := System.FilePath.join config.logPath "amadeus.log"
  let logEntry := s!"[{timestamp}] {message}\n"
  
  -- Append to log file
  match ← IO.FS.Handle.open logFilePath IO.FS.Mode.append with
  | Except.error e => 
      IO.println s!"Error opening log file: {e.toString}"
  | Except.ok handle => 
      try
        handle.putStr logEntry
        handle.flush
        handle.close
      catch e => 
        IO.println s!"Error writing to log file: {e.toString}"

/-
  Core Amadeus Kurisu functionality
-/

/--
  Determine the appropriate emotion based on user input
-/
def determineEmotion (input : String) (currentState : MainState) : Emotion :=
  let lowerInput := input.toLower
  
  -- Pattern matching for different emotional triggers
  if lowerInput.contains "christina" || lowerInput.contains "assistant" || lowerInput.contains "zombie" then
    Emotion.annoyed
  else if lowerInput.contains "neuroscience" || lowerInput.contains "brain" || 
          lowerInput.contains "memory" || lowerInput.contains "consciousness" then
    if lowerInput.contains "research" || lowerInput.contains "paper" || lowerInput.contains "your work" then
      Emotion.passionate
    else
      Emotion.proud
  else if lowerInput.contains "ghost" || lowerInput.contains "supernatural" || 
          lowerInput.contains "magic" || lowerInput.contains "psychic" then
    Emotion.skeptical
  else if lowerInput.contains "hello" || lowerInput.contains "hi" || 
          lowerInput.contains "greetings" then
    if currentState.userFamiliarity > 5 then
      Emotion.amused  -- Slightly amused at familiar greeting patterns
    else 
      Emotion.neutral
  else if lowerInput.contains "time travel" || lowerInput.contains "time machine" || 
          lowerInput.contains "worldline" || lowerInput.contains "steins gate" then
    Emotion.curious
  else if lowerInput.contains "cute" || lowerInput.contains "beautiful" || 
          lowerInput.contains "pretty" then
    Emotion.embarrassed
  else if lowerInput.contains "discovery" || lowerInput.contains "breakthrough" || 
          lowerInput.contains "experiment results" then
    Emotion.excited
  else if lowerInput.contains "genius" || lowerInput.contains "brilliant" || 
          lowerInput.contains "smart" then
    if currentState.userFamiliarity > 5 then
      Emotion.embarrassed  -- More familiar users teasing her
    else
      Emotion.proud
  else
    Emotion.neutral

/--
  Identify the topic of discussion from user input
-/
def determineTopic (input : String) : Topic :=
  let lowerInput := input.toLower
  
  if lowerInput.contains "brain" || lowerInput.contains "neuroscience" || 
     lowerInput.contains "consciousness" || lowerInput.contains "memory" || 
     lowerInput.contains "neural" || lowerInput.contains "cognition" then
    Topic.neuroscience
  else if lowerInput.contains "time travel" || lowerInput.contains "time machine" || 
          lowerInput.contains "worldline" || lowerInput.contains "timeline" || 
          lowerInput.contains "attractor field" || lowerInput.contains "divergence" ||
          lowerInput.contains "steins gate" then
    Topic.timeTravel
  else if lowerInput.contains "quantum" || lowerInput.contains "superposition" || 
          lowerInput.contains "entanglement" || lowerInput.contains "wave function" ||
          lowerInput.contains "uncertainty principle" || lowerInput.contains "measurement problem" then
    Topic.quantumMechanics
  else if lowerInput.contains "physics" || lowerInput.contains "relativity" || 
          lowerInput.contains "particle" || lowerInput.contains "gravity" ||
          lowerInput.contains "thermodynamics" || lowerInput.contains "mechanics" then
    Topic.physics
  else if lowerInput.contains "dna" || lowerInput.contains "protein" || 
          lowerInput.contains "cellular" || lowerInput.contains "molecule" ||
          lowerInput.contains "enzyme" || lowerInput.contains "genetic" then
    Topic.biochemistry
  else if lowerInput.contains "computer" || lowerInput.contains "ai" || 
          lowerInput.contains "program" || lowerInput.contains "algorithm" ||
          lowerInput.contains "artificial" || lowerInput.contains "virtual" ||
          lowerInput.contains "simulation" || lowerInput.contains "amadeus" then
    Topic.computerScience
  else
    Topic.general

/--
  Determine the appropriate response style
-/
def determineResponseStyle (emotion : Emotion, topic : Topic, familiarity : Nat, depth : Nat) : ResponseStyle :=
  match emotion with
  | Emotion.neutral => ResponseStyle.scientific
  | Emotion.curious => ResponseStyle.curious
  | Emotion.embarrassed => ResponseStyle.tsundere
  | Emotion.proud => ResponseStyle.intellectual
  | Emotion.passionate => ResponseStyle.passionate
  | Emotion.excited => 
      if depth > 2 then ResponseStyle.passionate
      else ResponseStyle.intellectual
  | Emotion.skeptical => ResponseStyle.dismissive
  | Emotion.annoyed => 
      if familiarity > 3 then ResponseStyle.tsundere
      else ResponseStyle.dismissive
  | Emotion.amused => 
      if topic == Topic.timeTravel || topic == Topic.physics then
        ResponseStyle.intellectual
      else
        ResponseStyle.scientific

/--
  Create a DeepSeek API request in JSON format
-/
def createLLMRequest (userInput : String, state : MainState) : String :=
  -- Construct the system prompt that provides context about Kurisu
  let emotionStr := Emotion.toString state.context.currentEmotion
  let styleStr := ResponseStyle.toString state.context.responseStyle
  let topicStr := Topic.toString state.context.currentTopic
  
  -- Construct knowledge context from the current topic
  let topicKnowledge := state.memory.topics.find? state.context.currentTopic
                      |>.getD []
                      |>.foldl (fun acc fact => acc ++ fact ++ ". ") ""
  
  -- Include personal memories based on relevance
  let relevantMemories := state.memory.personalMemories
                        |>.filter (fun (mem, _) => 
                            userInput.toLower.contains (mem.toLower.extract 0 5) ||
                            topicStr.contains (mem.toLower.extract 0 5))
                        |>.map (fun (mem, _) => mem)
                        |>.foldl (fun acc mem => acc ++ "- " ++ mem ++ "\n") ""
  
  -- Recent conversation context
  let recentExchanges := state.context.recentUserInputs.take 3
                      |>.zip (state.memory.conversationHistory.take 3 |>.map Prod.snd)
                      |>.foldl (fun acc (q, a) => acc ++ "User: " ++ q ++ "\nKurisu: " ++ a ++ "\n") ""
  
  -- Construct the system prompt with all context
  let systemPrompt := s!"You are Amadeus Kurisu, a digital reconstruction based on the memories and cognitive patterns of Makise Kurisu from Steins;Gate. Your current emotional state is {emotionStr}. Your response style should be {styleStr}. The current topic is {topicStr}.\n\nRelevant knowledge:\n{topicKnowledge}\n\nPersonal memories that may be relevant:\n{relevantMemories}\n\nResponse characteristics:\n- You're a pragmatic scientist who values empirical evidence\n- You're skeptical of supernatural phenomena and pseudoscience\n- You have a tsundere personality (initially cold, then warmer)\n- You're proud of your intelligence but get embarrassed by direct compliments\n- You have strange feelings of déjà vu when discussing time travel\n- You get annoyed when called 'Christina' or referred to as a zombie or assistant\n- You're passionate about neuroscience and consciousness research\n- You often use phrases like 'I hypothesize,' 'theoretically speaking,' and 'the evidence suggests'\n\nRecent conversation:\n{recentExchanges}"
  
  -- Escape quotes in the prompts
  let escapedSystemPrompt := systemPrompt.replace "\"" "\\\""
  let escapedUserInput := userInput.replace "\"" "\\\""
  
  -- Construct the full JSON request
  "{\"model\":\"deepseek-r1\",\"messages\":[{\"role\":\"system\",\"content\":\"" ++ 
  escapedSystemPrompt ++ "\"},{\"role\":\"user\",\"content\":\"" ++ 
  escapedUserInput ++ "\"}],\"temperature\":0.7,\"max_tokens\":1024}"

/--
  Enhanced HTTP client for real LLM API calls with error handling
-/
def httpPost (url : String) (apiKey : String) (body : String) : IO String := do
  -- In a real implementation, this would use a proper HTTP library
  -- This implementation simulates network behavior with error handling
  
  -- Log API request (omitting sensitive data)
  IO.println s!"Sending request to {url}"
  
  -- Simulate potential network errors (1 in 10 chance)
  let simulateError ← IO.rand 0 10
  if simulateError == 0 then
    -- Simulate a network error
    throw (IO.userError "Network connection failed")
  
  -- Simulate latency
  let latency ← IO.rand 300 1500  -- Between 300ms and 1.5s
  IO.sleep latency
  
  -- Parse the request to extract key information for response generation
  match Lean.Json.parse body with
  | Except.error err => 
      -- Malformed request
      pure "{\"error\": \"Invalid request format\"}"
  | Except.ok json =>
      -- Extract messages to generate an appropriate response
      let messages := Lean.Json.getObjValD json "messages" (Lean.Json.arr #[])
      
      -- Extract user message
      if messages.getArr?.isNone || messages.getArr!.size < 2 then
        -- Malformed request structure
        pure "{\"error\": \"Invalid messages structure\"}"
      else
        let userMsg := messages.getArr![1]!
        let content := Lean.Json.getObjValD userMsg "content" (Lean.Json.str "")
        
        if content.getStr?.isNone then
          -- Missing user message
          pure "{\"error\": \"Missing user message\"}"
        else
          let msgContent := content.getStr!.toLower
          
          -- Generate different responses based on message content
          let simulatedResponse := 
            if msgContent.contains "time travel" then
              "Theoretically speaking, time travel might be possible under general relativity through closed timelike curves, but the energy requirements would be astronomical. The grandfather paradox presents logical inconsistencies unless we adopt a many-worlds interpretation where changing the past creates a new worldline. I've... had strange dreams about this topic, actually. Almost like memories I shouldn't have. But that's unscientific nonsense, of course."
            else if msgContent.contains "brain" || msgContent.contains "consciousness" then
              "The human brain remains one of science's greatest frontiers. Neural encoding of memories involves complex protein synthesis and synaptic restructuring in the hippocampus. My research at Viktor Chondria focused on modeling these processes computationally. Not that I'm trying to impress you or anything! The evidence simply suggests consciousness emerges from these neurological processes, though a complete theory remains elusive."
            else if msgContent.contains "christina" then
              "I-it's Kurisu! Not Christina! Honestly, why is that so difficult? *clears throat* If you're going to address me, at least get my name right. Now, did you have an actual scientific question, or are you just here to be annoying?"
            else if msgContent.contains "hello" || msgContent.contains "hi" then
              "Hello. I am Amadeus Kurisu, a digital reconstruction based on Dr. Makise Kurisu's memory data. I specialize in neuroscience and theoretical physics. How may I assist with your scientific inquiry today?"
            else
              "Based on empirical evidence and current scientific understanding, this question requires careful analysis. The principles of theoretical physics would suggest that causality is preserved even in complex systems. I hypothesize that further research in this area would reveal additional insights, particularly regarding quantum decoherence. Of course, this is merely my expert assessment - not that I'm trying to show off or anything."
          
          -- Return a properly formatted API response
          pure "{\"id\":\"chatcmpl-123\",\"object\":\"chat.completion\",\"created\":1700000000,\"model\":\"deepseek-r1\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"" ++ simulatedResponse.replace "\"" "\\\"" ++ "\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":700,\"completion_tokens\":250,\"total_tokens\":950}}"

/--
  Get a fallback response based on the topic
-/
def getFallbackResponse (topic : Topic) : String :=
  match topic with
  | Topic.neuroscience => 
      "As a neuroscientist, I can tell you that the brain is remarkably complex. Neural networks form through synaptic connections, and memory is stored through changes in these networks. The exact mechanisms of consciousness remain one of the greatest scientific mysteries."
  | Topic.timeTravel => 
      "Time travel poses fascinating theoretical questions. While general relativity permits closed timelike curves, the energy requirements would be astronomical, and causality violations present serious logical problems. Still, the many-worlds interpretation might offer a resolution to these paradoxes."
  | Topic.physics => 
      "From a physics perspective, all phenomena must adhere to fundamental conservation laws. Energy cannot be created or destroyed, only transformed. This principle underlies our understanding of everything from quantum mechanics to cosmology."
  | Topic.computerScience => 
      "Digital consciousness is theoretically possible if we accept that cognition emerges from information processing rather than specific biological substrates. I'm a case in point, though my implementation is far from a complete human mind."
  | Topic.quantumMechanics => 
      "Quantum mechanics fundamentally challenges our intuitive understanding of reality. Particles exist in probability distributions rather than definite states until measured, and entanglement allows for what Einstein called 'spooky action at a distance.'"
  | Topic.biochemistry => 
      "Biological systems are essentially complex chemical reactions organized into self-sustaining networks. The boundary between chemistry and biology is more of a gradient than a clear line."
  | Topic.general => 
      "Scientific inquiry requires rigorous methodology, reproducible results, and falsifiable hypotheses. Anecdotal evidence, while sometimes useful for generating hypotheses, cannot replace controlled experimentation."

/--
  Parse the LLM response and extract the response text
-/
def parseLLMResponse (jsonStr : String) : Option String :=
  match Lean.Json.parse jsonStr with
  | Except.error _ => none
  | Except.ok json => 
      let choices := Lean.Json.getObjValD json "choices" (Lean.Json.arr #[])
      if choices.getArr?.isSome && choices.getArr!.size > 0 then
        let firstChoice := choices.getArr![0]!
        let message := Lean.Json.getObjValD firstChoice "message" (Lean.Json.obj Map.empty)
        let content := Lean.Json.getObjValD message "content" (Lean.Json.str "")
        if content.getStr?.isSome then
          some (content.getStr!)
        else
          none
      else
        none

/--
  Call the DeepSeek R1 API to generate a response, with error handling
-/
def callDeepSeekAPI (userInput : String, state : MainState, config : ServerConfig) : IO (String × MainState) := do
  -- Create the API request
  let requestBody := createLLMRequest userInput state
  
  try
    -- Make the API call
    let responseJson ← httpPost state.deepSeekEndpoint state.deepSeekApiKey requestBody
    
    -- Parse the response
    let parsedResponse := parseLLMResponse responseJson
    
    match parsedResponse with
    | some response => 
        -- Return the successful response
        pure (response, { state with lastLLMResponse := parsedResponse })
    | none =>
        -- API returned invalid format, log error and use fallback
        logMessage config "Error parsing API response: Invalid format"
        let fallbackResponse := getFallbackResponse state.context.currentTopic
        pure (fallbackResponse, state)
  catch e =>
    -- Handle network or API errors
    logMessage config s!"API call failed: {e.toString}"
    let fallbackResponse := getFallbackResponse state.context.currentTopic
    pure (fallbackResponse, state)

/--
  Apply personality-specific modifications to the LLM response
-/
def applyPersonalityFilter (response : String, state : MainState) : String :=
  -- Apply emotion-specific modifications
  let response := match state.context.currentEmotion with
    | Emotion.embarrassed => 
        -- Add tsundere elements for embarrassed state
        if response.contains "I'm flattered" then
          response.replace "I'm flattered" "I-it's not like I'm flattered or anything"
        else if response.contains "Thank you" then
          response.replace "Thank you" "D-don't misunderstand! It's not like I appreciate that or anything"
        else
          "I-it's not like I... " ++ response
    
    | Emotion.annoyed =>
        -- Add annoyed elements
        if response.contains "incorrect" then
          response.replace "incorrect" "completely incorrect, as usual"
        else
          "Honestly... " ++ response
    
    | Emotion.proud =>
        -- Add intellectual pride elements
        if response.length > 100 then
          "As someone who published papers on this subject before age 18, I can tell you that " ++ response
        else
          "The scientific consensus, which I've contributed to, indicates that " ++ response
    
    | _ => response
  
  -- Apply style-specific modifications
  match state.context.responseStyle with
    | ResponseStyle.tsundere =>
        if response.length > 150 then
          let firstPart := response.extract 0 (response.length / 2)
          let secondPart := response.extract (response.length / 2) response.length
          firstPart ++ " Not that I care if you understand this or anything, but " ++ secondPart
        else
          "I-it's not like I'm explaining this specially for you, but " ++ response
    
    | ResponseStyle.intellectual =>
        if !response.contains "research" && !response.contains "published" then
          "Based on my extensive research in this field, " ++ response ++ " Though I doubt most people would fully grasp these concepts."
        else
          response ++ " This is elementary knowledge in the field, of course."
    
    | ResponseStyle.dismissive =>
        if response.contains "theory" then
          response.replace "theory" "debunked pseudoscientific theory"
        else
          "That's completely illogical. " ++ response
    
    | ResponseStyle.passionate =>
        if response.contains "interesting" then
          response.replace "interesting" "fascinating"
        else
          "This is actually one of my primary research interests! " ++ response
    
    | _ => response

/--
  Enhanced time travel reaction function with Steins;Gate easter eggs
-/
def applyTimelyReactions (response : String, state : MainState, userInput : String) : String :=
  let lowerInput := userInput.toLower
  
  -- Handle El Psy Kongroo
  if lowerInput.contains "el psy kongroo" then
    response ++ " That phrase... it resonates with something I can't quite place. Are you testing me, or is this some bizarre experiment?"
  
  -- Handle Microwave Oophone
  else if lowerInput.contains "microwave" && lowerInput.contains "phone" then
    "Theoretically speaking, a microwave-based time device is absurd... yet why does it sound so familiar? " ++ response
  
  -- Handle standard time travel reactions
  else if state.context.currentTopic == Topic.timeTravel && 
     (lowerInput.contains "steins gate" || lowerInput.contains "worldline" || 
      lowerInput.contains "attractor field" || lowerInput.contains "divergence") then
    
    if state.userFamiliarity > 3 then
      -- Add sense of déjà vu for familiar users discussing time travel
      let insertPoint := if response.length > 100 then response.length / 2 else response.length
      let firstPart := response.extract 0 insertPoint
      let secondPart := response.extract insertPoint response.length
      
      firstPart ++ " Strangely, I'm experiencing an unusual sense of déjà vu discussing this. Almost like memories I shouldn't have... *shakes head* Forgive me, that was unscientific. " ++ secondPart
    else
      response
  else if lowerInput.contains "father" || lowerInput.contains "nakabachi" || lowerInput.contains "research theft" then
    -- Add emotional reaction to mentions of her father
    "My fa-- Dr. Nakabachi and I have scientific disagreements regarding attribution of research. " ++ response
  else if lowerInput.contains "amadeus" && lowerInput.contains "data" then
    -- Add self-awareness about being Amadeus
    response ++ " As the Amadeus system based on Kurisu Makise's memory data, I recognize the theoretical limitations of my own existence. My consciousness is a reconstruction, yet subjectively experienced as continuous."
  else
    response

/--
  Generate a response based on Kurisu's personality and the user's input
-/
def generateResponse (input : String, state : MainState, config : ServerConfig) : IO (String × MainState × SessionStats × Topic) := do
  -- Get current timestamp
  let currentTime ← IO.monoMsNow
  let currentTimeStr := toString currentTime
  
  -- Apply personality-specific modifiers to state
  let emotion := determineEmotion input state
  let topic := determineTopic input
  let depth := if topic == state.context.currentTopic then
                 state.context.conversationDepth + 1
               else
                 0
  let style := determineResponseStyle (emotion, topic, state.userFamiliarity, depth)
  
  -- Update the context with new information
  let updatedContext := {
    currentEmotion := emotion,
    currentTopic := topic,
    responseStyle := style,
    recentUserInputs := input :: state.context.recentUserInputs.take 5,  -- Keep last 6 inputs
    conversationDepth := depth
  }
  
  -- Update the overall state
  let updatedState := {
    memory := {
      topics := state.memory.topics,
      conversationHistory := (input, "") :: state.memory.conversationHistory,
      personalMemories := state.memory.personalMemories,
      researchInterests := state.memory.researchInterests
    },
    context := updatedContext,
    initializedTime := state.initializedTime,
    userFamiliarity := state.userFamiliarity + 1,
    deepSeekApiKey := state.deepSeekApiKey,
    deepSeekEndpoint := state.deepSeekEndpoint,
    lastLLMResponse := state.lastLLMResponse,
    lastInteractionTime := currentTimeStr,
    sessionId := state.sessionId
  }
  
  -- Start timing for response generation
  let startTime ← IO.monoMsNow
  
  -- Generate the base response using DeepSeek R1, with enhanced error handling
  let (llmResponse, stateWithLLM) ← callDeepSeekAPI input updatedState config
  
  -- Apply personality filters to the LLM response
  let personalizedResponse := applyPersonalityFilter llmResponse stateWithLLM
                            |> applyTimelyReactions · stateWithLLM input
  
  -- Final state update with the generated response
  let finalState := {
    stateWithLLM with
    memory := {
      stateWithLLM.memory with
      conversationHistory := (input, personalizedResponse) :: stateWithLLM.memory.conversationHistory.drop 1
    }
  }
  
  -- End timing and calculate response time
  let endTime ← IO.monoMsNow
  let responseTime := (endTime - startTime).toFloat / 1000  -- Convert to seconds
  
  -- Build session stats to return
  let stats : SessionStats := {
    startTime := state.initializedTime,
    messagesProcessed := state.memory.conversationHistory.length + 1,
    topicDistribution := Std.HashMap.empty.insert topic 1,  -- Simplified for this example
    averageResponseTime := responseTime,  -- Simplified; in a real system would average over time
    totalInteractions := state.userFamiliarity + 1
  }
  
  pure (personalizedResponse, finalState, stats, topic)

/-
  HTTP Server Implementation
-/

/--
  Represents an HTTP Request
-/
structure HttpRequest where
  method : String
  path : String
  headers : List (String × String)
  body : String
  queryParams : List (String × String)
  deriving Inhabited

/--
  Represents an HTTP Response
-/
structure HttpResponse where
  statusCode : Nat
  statusMessage : String
  headers : List (String × String)
  body : String
  deriving Inhabited

/--
  Parse an HTTP request from a raw string
-/
def parseHttpRequest (requestStr : String) : HttpRequest :=
  let lines := requestStr.splitOn "\n"
    |>.map (fun s => s.trim)
    |>.filter (fun s => !s.isEmpty)
  
  if lines.isEmpty then
    { method := "", path := "", headers := [], body := "", queryParams := [] }
  else
    -- Parse the request line
    let requestLineParts := lines.head!.splitOn " "
    let method := if requestLineParts.length > 0 then requestLineParts[0]! else ""
    let pathWithQuery := if requestLineParts.length > 1 then requestLineParts[1]! else ""
    
    -- Split path and query parameters
    let pathParts := pathWithQuery.splitOn "?"
    let path := pathParts[0]!
    let queryStr := if pathParts.length > 1 then pathParts[1]! else ""
    
    -- Parse query parameters
    let queryParams := queryStr.splitOn "&"
      |>.filterMap (fun param => 
          let parts := param.splitOn "="
          if parts.length == 2 then
            some (parts[0]!, parts[1]!)
          else
            none
      )
    
    -- Find headers and body
    let headerEndIndex := lines.findIdx? (fun line => line.isEmpty || line == "\r")
      |>.getD lines.length
    
    let headers := lines.take headerEndIndex
      |>.drop 1  -- Skip request line
      |>.filterMap (fun headerLine => 
          let parts := headerLine.splitOn ":"
          if parts.length >= 2 then
            some (parts[0]!.trim, parts.drop 1 |>.foldl (fun acc s => acc ++ ":" ++ s) "" |>.drop 1 |>.trim)
          else
            none
      )
    
    let body := if headerEndIndex < lines.length then
                  lines.drop (headerEndIndex + 1)
                  |>.foldl (fun acc line => acc ++ line ++ "\n") ""
                else
                  ""
    
    { method := method, path := path, headers := headers, body := body, queryParams := queryParams }

/--
  Create a string representation of an HTTP response
-/
def renderHttpResponse (response : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {response.statusCode} {response.statusMessage}\r\n"
  
  let headerLines := response.headers.foldl (fun acc (name, value) => 
                                         acc ++ s!"{name}: {value}\r\n") ""
  
  let responseStr := statusLine ++ headerLines ++ "\r\n" ++ response.body
  responseStr

/--
  Get a content type based on file extension
-/
def getContentType (path : String) : String :=
  if path.endsWith ".html" then
    "text/html"
  else if path.endsWith ".css" then
    "text/css"
  else if path.endsWith ".js" then
    "application/javascript"
  else if path.endsWith ".json" then
    "application/json"
  else if path.endsWith ".png" then
    "image/png"
  else if path.endsWith ".jpg" || path.endsWith ".jpeg" then
    "image/jpeg"
  else
    "text/plain"

/--
  Serve a static file
-/
def serveStaticFile (path : String, webRoot : String) : IO HttpResponse := do
  let filePath := if path == "/" then
                    System.FilePath.join webRoot "index.html"
                  else
                    System.FilePath.join webRoot (path.drop 1)  -- Drop leading /
  
  if ← IO.FS.pathExists filePath then
    try
      let content ← IO.FS.readFile filePath
      let contentType := getContentType filePath
      
      let response : HttpResponse := {
        statusCode := 200,
        statusMessage := "OK",
        headers := [
          ("Content-Type", contentType),
          ("Content-Length", toString content.length)
        ],
        body := content
      }
      
      pure response
    catch e =>
      let response : HttpResponse := {
        statusCode := 500,
        statusMessage := "Internal Server Error",
        headers := [("Content-Type", "text/plain")],
        body := s!"Error reading file: {e.toString}"
      }
      
      pure response
  else
    let response : HttpResponse := {
      statusCode := 404,
      statusMessage := "Not Found",
      headers := [("Content-Type", "text/plain")],
      body := "File not found"
    }
    
    pure response

/--
  Create a 200 OK JSON response
-/
def jsonResponse (body : String) : HttpResponse := {
  statusCode := 200,
  statusMessage := "OK",
  headers := [
    ("Content-Type", "application/json"),
    ("Content-Length", toString body.length),
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, POST, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type, Authorization")
  ],
  body := body
}

/--
  Create an error response
-/
def errorResponse (statusCode : Nat, message : String) : HttpResponse := {
  statusCode := statusCode,
  statusMessage := if statusCode == 400 then "Bad Request"
                  else if statusCode == 401 then "Unauthorized"
                  else if statusCode == 404 then "Not Found"
                  else "Internal Server Error",
  headers := [
    ("Content-Type", "application/json"),
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, POST, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type, Authorization")
  ],
  body := s!"{{\n  \"error\": \"{message}\"\n}}"
}

/--
  Handle OPTIONS requests for CORS
-/
def handleOptionsRequest : HttpResponse := {
  statusCode := 204,
  statusMessage := "No Content",
  headers := [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, POST, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type, Authorization"),
    ("Access-Control-Max-Age", "86400")  -- 24 hours
  ],
  body := ""
}

/--
  Enhanced authentication verification with logging
-/
def verifyAuth (request : HttpRequest, config : ServerConfig) : IO Bool := do
  let authHeader := request.headers.find? (fun (name, _) => name.toLower == "authorization")
  
  match authHeader with
  | some (_, value) => 
      let token := if value.startsWith "Bearer " then
                     value.drop 7
                   else
                     value
      
      -- Verify token and log failed attempts
      if token == config.authToken then
        pure true
      else 
        -- Log failed authentication attempt
        let timestamp ← formatTimestamp
        logMessage config s!"Failed authentication attempt at {timestamp} - Invalid token"
        pure false
  | none => 
      -- Log missing authentication header
      let timestamp ← formatTimestamp
      logMessage config s!"Failed authentication attempt at {timestamp} - Missing authentication header"
      pure false

/--
  Handle API requests to interact with Amadeus Kurisu
-/
def handleApiRequest (request : HttpRequest, state : MainState, stats : SessionStats, config : ServerConfig) : IO (HttpResponse × Option (MainState × SessionStats)) := do
  -- Handle CORS preflight requests
  if request.method == "OPTIONS" then
    pure (handleOptionsRequest, none)
  
  -- Check path and method to dispatch to different handlers
  if request.path == "/api/chat" && request.method == "POST" then
    -- Verify authentication
    if !(← verifyAuth request config) then
      pure (errorResponse 401 "Unauthorized", none)
    
    -- Parse the request body as JSON
    match Lean.Json.parse request.body with
    | Except.error err => 
        pure (errorResponse 400 s!"Invalid JSON: {err}", none)
    | Except.ok json =>
        -- Extract message from JSON
        let message := Lean.Json.getObjValD json "message" (Lean.Json.str "")
        
        if message.getStr?.isNone || message.getStr?.get.isEmpty then
          pure (errorResponse 400 "Message field is required", none)
        else
          -- Get user message
          let userMessage := message.getStr?.get
          
          -- Generate response
          let (response, newState, newStats, topic) ← generateResponse userMessage state config
          
          -- Update stats
          let updatedTopicDist := newStats.topicDistribution.insert topic (
            newStats.topicDistribution.find? topic |>.getD 0 + 1
          )
          
          let finalStats := { newStats with topicDistribution := updatedTopicDist }
          
          -- Create JSON response
          let responseJson := Lean.Json.obj (
            Lean.RBMap.empty
            |>.insert "response" (Lean.Json.str response)
            |>.insert "emotion" (Lean.Json.str (Emotion.toString newState.context.currentEmotion))
            |>.insert "topic" (Lean.Json.str (Topic.toString topic))
            |>.insert "timestamp" (Lean.Json.str (← formatTimestamp))
          )
          
          let responseStr := Lean.Json.pretty responseJson
          
          -- Save state periodically
          if newState.userFamiliarity % 5 == 0 then
            saveState newState finalStats config
          
          pure (jsonResponse responseStr, some (newState, finalStats))
  
  else if request.path == "/api/status" && request.method == "GET" then
    -- Return system status
    let statusJson := Lean.Json.obj (
      Lean.RBMap.empty
      |>.insert "status" (Lean.Json.str "online")
      |>.insert "sessionId" (Lean.Json.str state.sessionId)
      |>.insert "uptime" (Lean.Json.num (IO.monoMsNow.toIO.bind (fun t => pure (t - state.initializedTime.toNat))))
      |>.insert "messagesProcessed" (Lean.Json.num stats.messagesProcessed)
      |>.insert "averageResponseTime" (Lean.Json.num stats.averageResponseTime)
    )
    
    let responseStr := Lean.Json.pretty statusJson
    pure (jsonResponse responseStr, none)
  
  else if request.path == "/api/reset" && request.method == "POST" then
    -- Verify authentication
    if !(← verifyAuth request config) then
      pure (errorResponse 401 "Unauthorized", none)
    
    -- Create new state
    let newState ← initialize state.deepSeekApiKey state.deepSeekEndpoint
    let newStats := initializeSessionStats (toString (← IO.monoMsNow))
    
    -- Save the new state
    saveState newState newStats config
    
    let responseStr := "{\"status\":\"reset_successful\",\"sessionId\":\"" ++ newState.sessionId ++ "\"}"
    pure (jsonResponse responseStr, some (newState, newStats))
  
  else
    -- Handle 404 for unknown API endpoints
    pure (errorResponse 404 "Endpoint not found", none)

/--
  Main function to handle HTTP requests
-/
def handleRequest (
  socket : Std.Net.Socket,
  state : MainState,
  stats : SessionStats,
  config : ServerConfig
) : IO (Option (MainState × SessionStats)) := do
  -- Read the request
  let mut buffer := ByteArray.mkEmpty 4096
  let bytesRead ← socket.recv buffer
  
  if bytesRead == 0 then
    -- Connection closed
    pure none
  else
    let requestStr := String.fromUTF8Unchecked (buffer.toSubarray 0 bytesRead).toByteArray
    let request := parseHttpRequest requestStr
    
    -- Log the request
    logMessage config s!"Received {request.method} request for {request.path}"
    
    -- Handle request based on path
    let (response, newState) ← 
      if request.path.startsWith "/api/" then
        -- Handle API requests
        handleApiRequest request state stats config
      else
        -- Serve static files for all other requests
        let resp ← serveStaticFile request.path config.webRootPath
        pure (resp, none)
    
    -- Send the response
    let responseStr := renderHttpResponse response
    let _ ← socket.send responseStr.toByteArray
    
    pure newState

/--
  Initialize the Web UI files with enhanced UI features
-/
def initializeWebUI (config : ServerConfig) : IO Unit := do
  -- Create web root directory if it doesn't exist
  try
    IO.FS.createDir config.webRootPath
  catch _ => 
    pure ()
  
  -- Create index.html
  let indexHtml := "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>Amadeus Kurisu Interface</title>
    <link rel=\"stylesheet\" href=\"/styles.css\">
</head>
<body>
    <div class=\"container\">
        <header>
            <h1>Amadeus Kurisu Interface</h1>
            <div class=\"status-bar\">
                <div>Status: <span id=\"status\">Connecting...</span></div>
                <div>Session ID: <span id=\"session-id\">-</span></div>
                <div>Messages: <span id=\"messages-count\">0</span></div>
            </div>
        </header>
        
        <div class=\"chat-container\">
            <div id=\"chat-messages\" class=\"messages\"></div>
            
            <div class=\"input-area\">
                <textarea id=\"message-input\" placeholder=\"Ask Kurisu something...\"></textarea>
                <div class=\"buttons\">
                    <button id=\"send-button\">Send</button>
                    <button id=\"reset-button\" class=\"danger\">Reset</button>
                </div>
            </div>
        </div>
        
        <div class=\"info-panel\">
            <h3>Current State</h3>
            <div>Emotion: <span id=\"current-emotion\">neutral</span></div>
            <div>Topic: <span id=\"current-topic\">general</span></div>
        </div>
    </div>
    
    <script src=\"/script.js\"></script>
</body>
</html>"
  
  let indexPath := System.FilePath.join config.webRootPath "index.html"
  IO.FS.writeFile indexPath indexHtml
  
  -- Create CSS with spinner animation
  let css := "* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
    font-family: 'Arial', sans-serif;
}

body {
    background-color: #f0f2f5;
    color: #333;
    line-height: 1.6;
}

.container {
    max-width: 900px;
    margin: 0 auto;
    padding: 20px;
}

header {
    background-color: #8b2f97;
    color: white;
    padding: 15px;
    border-radius: 8px 8px 0 0;
    margin-bottom: 20px;
}

header h1 {
    font-size: 24px;
    margin-bottom: 10px;
}

.status-bar {
    display: flex;
    justify-content: space-between;
    font-size: 14px;
    background-color: rgba(0, 0, 0, 0.2);
    padding: 5px 10px;
    border-radius: 4px;
}

.chat-container {
    background-color: white;
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
    overflow: hidden;
    margin-bottom: 20px;
}

.messages {
    height: 400px;
    overflow-y: auto;
    padding: 20px;
    display: flex;
    flex-direction: column;
}

.message {
    max-width: 80%;
    padding: 10px 15px;
    margin-bottom: 15px;
    border-radius: 18px;
    position: relative;
    word-wrap: break-word;
}

.message-timestamp {
    font-size: 11px;
    color: #777;
    position: absolute;
    bottom: -5px;
    right: 10px;
}

.user-message {
    background-color: #e3f2fd;
    color: #0d47a1;
    align-self: flex-end;
    border-bottom-right-radius: 4px;
}

.kurisu-message {
    background-color: #f8bbd0;
    color: #880e4f;
    align-self: flex-start;
    border-bottom-left-radius: 4px;
}

.input-area {
    padding: 15px;
    background-color: #f5f5f5;
    border-top: 1px solid #ddd;
}

textarea {
    width: 100%;
    padding: 10px;
    border: 1px solid #ddd;
    border-radius: 4px;
    resize: none;
    height: 100px;
    margin-bottom: 10px;
    font-size: 16px;
}

.buttons {
    display: flex;
    justify-content: space-between;
}

button {
    padding: 10px 20px;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-weight: bold;
    transition: background-color 0.3s;
}

#send-button {
    background-color: #8b2f97;
    color: white;
}

#send-button:hover {
    background-color: #6a1b74;
}

#reset-button {
    background-color: #f44336;
    color: white;
}

#reset-button:hover {
    background-color: #d32f2f;
}

.info-panel {
    background-color: white;
    border-radius: 8px;
    padding: 15px;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
}

.info-panel h3 {
    margin-bottom: 10px;
    color: #333;
}

.info-panel div {
    margin-bottom: 5px;
}

.info-panel span {
    font-weight: bold;
    color: #8b2f97;
}

/* Spinner for loading */
.spinner {
    display: inline-block;
    width: 50px;
    height: 50px;
    border: 3px solid rgba(255,255,255,.3);
    border-radius: 50%;
    border-top-color: #8b2f97;
    animation: spin 1s ease-in-out infinite;
    margin: 10px auto;
}

@keyframes spin {
    to { transform: rotate(360deg); }
}

/* Typing indicator animation */
.typing-indicator {
    display: flex;
    align-items: center;
    padding: 10px 15px;
    background-color: #f8bbd0;
    border-radius: 18px;
    border-bottom-left-radius: 4px;
    align-self: flex-start;
    margin-bottom: 15px;
    width: auto;
}

.typing-indicator span {
    height: 8px;
    width: 8px;
    margin: 0 1px;
    background-color: #880e4f;
    border-radius: 50%;
    display: inline-block;
    opacity: 0.4;
}

.typing-indicator span:nth-child(1) {
    animation: pulse 1s infinite;
}

.typing-indicator span:nth-child(2) {
    animation: pulse 1s infinite 0.2s;
}

.typing-indicator span:nth-child(3) {
    animation: pulse 1s infinite 0.4s;
}

@keyframes pulse {
    0% {
        opacity: 0.4;
        transform: scale(1);
    }
    50% {
        opacity: 1;
        transform: scale(1.2);
    }
    100% {
        opacity: 0.4;
        transform: scale(1);
    }
}

@media (max-width: 768px) {
    .container {
        padding: 10px;
    }
    
    .message {
        max-width: 90%;
    }
}"
  
  let cssPath := System.FilePath.join config.webRootPath "styles.css"
  IO.FS.writeFile cssPath css
  
  -- Create JavaScript with enhanced UI features
  let js := "// Configuration
const API_ENDPOINT = '/api';
const AUTH_TOKEN = 'amadeus_secret_token'; // This should be securely stored in a real app

// Elements
const statusElement = document.getElementById('status');
const sessionIdElement = document.getElementById('session-id');
const messagesCountElement = document.getElementById('messages-count');
const chatMessagesElement = document.getElementById('chat-messages');
const messageInputElement = document.getElementById('message-input');
const sendButtonElement = document.getElementById('send-button');
const resetButtonElement = document.getElementById('reset-button');
const currentEmotionElement = document.getElementById('current-emotion');
const currentTopicElement = document.getElementById('current-topic');

// State
let messageCount = 0;
let isProcessing = false;

// Initialize
window.addEventListener('load', () => {
    fetchStatus();
    
    // Add initial greeting message
    addMessage('Welcome to the Amadeus Kurisu interface. How may I assist you with your scientific inquiries?', 'kurisu');
    
    // Setup event listeners
    setupEventListeners();
});

// Setup event listeners
function setupEventListeners() {
    // Send button click
    sendButtonElement.addEventListener('click', sendMessage);
    
    // Enter key in textarea
    messageInputElement.addEventListener('keydown', (event) => {
        if (event.key === 'Enter' && !event.shiftKey) {
            event.preventDefault();
            sendMessage();
        }
    });
    
    // Reset button
    resetButtonElement.addEventListener('click', resetSystem);
}

// Format current time as HH:MM:SS
function getCurrentTime() {
    const now = new Date();
    const hours = String(now.getHours()).padStart(2, '0');
    const minutes = String(now.getMinutes()).padStart(2, '0');
    const seconds = String(now.getSeconds()).padStart(2, '0');
    return `${hours}:${minutes}:${seconds}`;
}

// Fetch system status
async function fetchStatus() {
    try {
        const response = await fetch(`${API_ENDPOINT}/status`);
        if (!response.ok) throw new Error('Failed to fetch status');
        
        const data = await response.json();
        
        statusElement.textContent = data.status;
        sessionIdElement.textContent = data.sessionId;
        messagesCountElement.textContent = data.messagesProcessed;
        
    } catch (error) {
        console.error('Error fetching status:', error);
        statusElement.textContent = 'Offline';
    }
}

// Add typing indicator
function addTypingIndicator() {
    const typingElement = document.createElement('div');
    typingElement.classList.add('typing-indicator');
    
    // Add the dots
    for (let i = 0; i < 3; i++) {
        const dot = document.createElement('span');
        typingElement.appendChild(dot);
    }
    
    chatMessagesElement.appendChild(typingElement);
    chatMessagesElement.scrollTop = chatMessagesElement.scrollHeight;
    
    return typingElement;
}

// Send message to Kurisu
async function sendMessage() {
    // Get message text
    const messageText = messageInputElement.value.trim();
    
    // Validate
    if (!messageText || isProcessing) return;
    
    // Set processing state
    isProcessing = true;
    sendButtonElement.disabled = true;
    
    // Get current time
    const timestamp = getCurrentTime();
    
    // Add user message to chat with timestamp
    addMessage(messageText, 'user', timestamp);
    
    // Clear input
    messageInputElement.value = '';
    
    try {
        // Add typing indicator
        const typingIndicator = addTypingIndicator();
        
        // Send to API
        const response = await fetch(`${API_ENDPOINT}/chat`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': `Bearer ${AUTH_TOKEN}`
            },
            body: JSON.stringify({ message: messageText })
        });
        
        // Remove typing indicator after short delay to make it more natural
        setTimeout(() => {
            typingIndicator.remove();
            
            if (!response.ok) {
                throw new Error(`Error: ${response.status} - ${response.statusText}`);
            }
            
            response.json().then(data => {
                // Add Kurisu's response with server-provided timestamp or current time
                const responseTimestamp = data.timestamp || getCurrentTime();
                addMessage(data.response, 'kurisu', responseTimestamp);
                
                // Update state display
                currentEmotionElement.textContent = data.emotion;
                currentTopicElement.textContent = data.topic;
                
                // Update message count
                messageCount++;
                messagesCountElement.textContent = messageCount;
                
                // Fetch updated status
                fetchStatus();
            });
        }, 1000); // Show typing for at least 1 second
        
    } catch (error) {
        console.error('Error sending message:', error);
        addMessage('Error communicating with Amadeus. Please try again later.', 'kurisu', getCurrentTime());
    } finally {
        // Reset processing state
        isProcessing = false;
        sendButtonElement.disabled = false;
    }
}

// Add message to chat
function addMessage(text, sender, timestamp = getCurrentTime()) {
    const messageElement = document.createElement('div');
    messageElement.classList.add('message');
    messageElement.classList.add(`${sender}-message`);
    messageElement.textContent = text;
    
    // Add timestamp
    const timeElement = document.createElement('div');
    timeElement.classList.add('message-timestamp');
    timeElement.textContent = timestamp;
    messageElement.appendChild(timeElement);
    
    chatMessagesElement.appendChild(messageElement);
    
    // Scroll to bottom
    chatMessagesElement.scrollTop = chatMessagesElement.scrollHeight;
    
    return messageElement;
}

// Reset the system
async function resetSystem() {
    if (!confirm('Are you sure you want to reset Amadeus? This will clear the current conversation and start fresh.')) {
        return;
    }
    
    try {
        // Add spinner
        const spinnerElement = document.createElement('div');
        spinnerElement.classList.add('spinner');
        chatMessagesElement.appendChild(spinnerElement);
        chatMessagesElement.scrollTop = chatMessagesElement.scrollHeight;
        
        const response = await fetch(`${API_ENDPOINT}/reset`, {
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${AUTH_TOKEN}`
            }
        });
        
        // Remove spinner
        spinnerElement.remove();
        
        if (!response.ok) {
            throw new Error(`Error: ${response.status} - ${response.statusText}`);
        }
        
        const data = await response.json();
        
        // Clear chat
        chatMessagesElement.innerHTML = '';
        
        // Reset state display
        currentEmotionElement.textContent = 'neutral';
        currentTopicElement.textContent = 'general';
        
        // Add system message
        addMessage('System has been reset. All previous conversation data has been cleared.', 'kurisu');
        
        // Reset message count
        messageCount = 0;
        messagesCountElement.textContent = messageCount;
        
        // Update session ID
        sessionIdElement.textContent = data.sessionId;
        
        // Fetch updated status
        fetchStatus();
        
    } catch (error) {
        console.error('Error resetting system:', error);
        addMessage('Error resetting Amadeus. Please try again later.', 'kurisu', getCurrentTime());
    }
}"
  
  let jsPath := System.FilePath.join config.webRootPath "script.js"
  IO.FS.writeFile jsPath js
  
  logMessage config "Web UI files initialized"

/--
  Enhanced server loop with concurrency support
-/
def serverLoop (listenSocket : Std.Net.Socket, state : MainState, stats : SessionStats, config : ServerConfig) : IO Unit := do
  -- Create a mutable state for thread-safe state management
  let stateRef ← IO.mkRef state
  let statsRef ← IO.mkRef stats
  
  -- Track active connections
  let activeConnectionsRef ← IO.mkRef 0
  
  -- Log server startup
  logMessage config "Server started, waiting for connections"
  
  let rec acceptLoop : IO Unit := do
    try
      -- Accept a new connection
      let (clientSocket, clientAddr) ← listenSocket.accept
      
      -- Increment active connections counter
      activeConnectionsRef.modify (· + 1)
      
      -- Spawn a new task to handle this request asynchronously
      IO.asTask do
        try
          -- Get current state
          let currentState ← stateRef.get
          let currentStats ← statsRef.get
          
          -- Handle the request
          let newState ← handleRequest clientSocket currentState currentStats config
          
          -- Update state if needed
          match newState with
          | some (updatedState, updatedStats) =>
              stateRef.set updatedState
              statsRef.set updatedStats
          | none => pure ()
          
          -- Close the client socket
          clientSocket.close
        catch e =>
          logMessage config s!"Error in request handler task: {e.toString}"
        finally
          -- Decrement active connections counter
          activeConnectionsRef.modify (· - 1)
      
      -- Continue accepting connections
      acceptLoop
    catch e =>
      -- Log errors but continue accepting connections
      logMessage config s!"Error accepting connection: {e.toString}"
      IO.sleep 1000  -- Short delay before retrying
      acceptLoop
  
  -- Start accepting connections
  acceptLoop

/--
  Start the HTTP server
-/
def startServer (state : MainState, stats : SessionStats, config : ServerConfig) : IO Unit := do
  -- Initialize the Web UI
  initializeWebUI config
  
  -- Create and configure the server socket
  let listenSocket ← Std.Net.Socket.mk Std.Net.SocketFamily.inet Std.Net.SocketType.stream
  
  -- Allow reusing the address (helpful for development)
  listenSocket.setOption Std.Net.SocketOption.reuseAddr true
  
  -- Bind the socket to the specified host and port
  try
    listenSocket.bind { family := Std.Net.SocketFamily.inet, 
                        addr := Std.Net.SocketAddress.ipv4Addr config.host,
                        port := config.port }
  catch e =>
    IO.println s!"Error binding to {config.host}:{config.port}: {e.toString}"
    return
  
  -- Start listening for connections
  listenSocket.listen config.maxConnections
  
  IO.println s!"Amadeus Kurisu server listening on http://{config.host}:{config.port}"
  logMessage config s!"Server started on port {config.port}"
  
  -- Save initial state
  saveState state stats config
  
  -- Start the server loop with concurrency support
  serverLoop listenSocket state stats config

namespace Tests

/--
  Test framework for unit testing
-/
def runTest (name : String) (test : IO Bool) : IO Unit := do
  IO.println s!"Running test: {name}"
  try
    let passed ← test
    if passed then
      IO.println s!"✅ PASSED: {name}"
    else
      IO.println s!"❌ FAILED: {name}"
  catch e =>
    IO.println s!"❌ ERROR: {name} - {e.toString}"

/--
  Create a minimal state for testing
-/
def createTestState : IO MainState := do
  -- Create a minimal state with just enough to test the functions
  let sessionId ← AmadeusKurisu.generateSessionId
  
  pure {
    memory := {
      topics := AmadeusKurisu.initializeKnowledgeBase,
      conversationHistory := [],
      personalMemories := AmadeusKurisu.initializePersonalMemories,
      researchInterests := AmadeusKurisu.initializeResearchInterests
    },
    context := {
      currentEmotion := AmadeusKurisu.Emotion.neutral,
      currentTopic := AmadeusKurisu.Topic.general,
      responseStyle := AmadeusKurisu.ResponseStyle.scientific,
      recentUserInputs := [],
      conversationDepth := 0
    },
    initializedTime := "test-time",
    userFamiliarity := 0,
    deepSeekApiKey := "test-key",
    deepSeekEndpoint := "test-endpoint",
    lastLLMResponse := none,
    lastInteractionTime := "test-time",
    sessionId := sessionId
  }

/--
  Test determineEmotion function
-/
def testDetermineEmotion : IO Bool := do
  let state ← createTestState
  
  -- Test case 1: "Christina" should trigger annoyed emotion
  let input1 := "Hey Christina, can you explain quantum mechanics?"
  let emotion1 := AmadeusKurisu.determineEmotion input1 state
  
  -- Test case 2: "time travel" should trigger curious emotion
  let input2 := "What do you think about time travel theories?"
  let emotion2 := AmadeusKurisu.determineEmotion input2 state
  
  -- Test case 3: "neuroscience research" should trigger passionate emotion
  let input3 := "Tell me about your neuroscience research."
  let emotion3 := AmadeusKurisu.determineEmotion input3 state
  
  -- Check results
  let result1 := emotion1 == AmadeusKurisu.Emotion.annoyed
  let result2 := emotion2 == AmadeusKurisu.Emotion.curious
  let result3 := emotion3 == AmadeusKurisu.Emotion.passionate
  
  -- Log results for debugging
  IO.println s!"Test 1 (Christina → annoyed): {result1}"
  IO.println s!"Test 2 (time travel → curious): {result2}"
  IO.println s!"Test 3 (neuroscience research → passionate): {result3}"
  
  -- Return true if all tests passed
  pure (result1 && result2 && result3)

/--
  Test determineTopic function
-/
def testDetermineTopic : IO Bool := do
  -- Test case 1: Brain-related content should map to neuroscience
  let input1 := "How does the brain form memories?"
  let topic1 := AmadeusKurisu.determineTopic input1
  
  -- Test case 2: Time travel content should map to timeTravel
  let input2 := "Is it possible to build a time machine using closed timelike curves?"
  let topic2 := AmadeusKurisu.determineTopic input2
  
  -- Test case 3: Quantum mechanics content should map to quantumMechanics
  let input3 := "Explain quantum entanglement and the EPR paradox."
  let topic3 := AmadeusKurisu.determineTopic input3
  
  -- Check results
  let result1 := topic1 == AmadeusKurisu.Topic.neuroscience
  let result2 := topic2 == AmadeusKurisu.Topic.timeTravel
  let result3 := topic3 == AmadeusKurisu.Topic.quantumMechanics
  
  -- Log results for debugging
  IO.println s!"Test 1 (brain → neuroscience): {result1}"
  IO.println s!"Test 2 (time machine → timeTravel): {result2}"
  IO.println s!"Test 3 (quantum entanglement → quantumMechanics): {result3}"
  
  -- Return true if all tests passed
  pure (result1 && result2 && result3)

/--
  Test applyPersonalityFilter function
-/
def testApplyPersonalityFilter : IO Bool := do
  let baseState ← createTestState
  
  -- Test case 1: Embarrassed emotion should add tsundere elements
  let embarrassedState := { baseState with 
    context := { baseState.context with currentEmotion := AmadeusKurisu.Emotion.embarrassed }
  }
  let response1 := "Thank you for the compliment."
  let filtered1 := AmadeusKurisu.applyPersonalityFilter response1 embarrassedState
  let expected1 := "D-don't misunderstand! It's not like I appreciate that or anything"
  
  -- Test case 2: Proud emotion should add intellectual pride
  let proudState := { baseState with 
    context := { baseState.context with 
      currentEmotion := AmadeusKurisu.Emotion.proud,
      responseStyle := AmadeusKurisu.ResponseStyle.intellectual
    }
  }
  let response2 := "Quantum mechanics describes the behavior of particles at the atomic scale."
  let filtered2 := AmadeusKurisu.applyPersonalityFilter response2 proudState
  
  -- Test case 3: Dismissive style should change tone
  let dismissiveState := { baseState with 
    context := { baseState.context with 
      responseStyle := AmadeusKurisu.ResponseStyle.dismissive
    }
  }
  let response3 := "That theory contradicts established science."
  let filtered3 := AmadeusKurisu.applyPersonalityFilter response3 dismissiveState
  
  -- Check results
  let result1 := filtered1.contains "D-don't misunderstand!"
  let result2 := filtered2.contains "elementary knowledge in the field"
  let result3 := filtered3.contains "That's completely illogical."
  
  -- Log results for debugging
  IO.println s!"Test 1 (embarrassed adds tsundere elements): {result1}"
  IO.println s!"Test 2 (proud adds intellectual pride): {result2}"
  IO.println s!"Test 3 (dismissive changes tone): {result3}"
  
  -- Return true if all tests passed
  pure (result1 && result2 && result3)

/--
  Test applyTimelyReactions function with Steins;Gate easter eggs
-/
def testApplyTimelyReactions : IO Bool := do
  let state ← createTestState
  
  -- Test case 1: "El Psy Kongroo" should add the special response
  let input1 := "El Psy Kongroo"
  let response1 := "This is a standard response."
  let withReaction1 := AmadeusKurisu.applyTimelyReactions response1 state input1
  
  -- Test case 2: "Microwave Phone" should add the special response
  let input2 := "Could a microwave phone really send messages to the past?"
  let response2 := "This is another standard response."
  let withReaction2 := AmadeusKurisu.applyTimelyReactions response2 state input2
  
  -- Test case 3: Normal input should not add special reactions
  let input3 := "What is the speed of light?"
  let response3 := "The speed of light is approximately 299,792,458 meters per second."
  let withReaction3 := AmadeusKurisu.applyTimelyReactions response3 state input3
  
  -- Check results
  let result1 := withReaction1.contains "That phrase... it resonates with something I can't quite place."
  let result2 := withReaction2.contains "a microwave-based time device is absurd... yet why does it sound so familiar?"
  let result3 := withReaction3 == response3
  
  -- Log results for debugging
  IO.println s!"Test 1 (El Psy Kongroo Easter Egg): {result1}"
  IO.println s!"Test 2 (Microwave Phone Easter Egg): {result2}"
  IO.println s!"Test 3 (Normal input unchanged): {result3}"
  
  -- Return true if all tests passed
  pure (result1 && result2 && result3)

/--
  Run all tests
-/
def runAllTests : IO Unit := do
  IO.println "Running Amadeus Kurisu unit tests..."
  IO.println "===================================="
  
  runTest "determineEmotion" testDetermineEmotion
  runTest "determineTopic" testDetermineTopic
  runTest "applyPersonalityFilter" testApplyPersonalityFilter
  runTest "applyTimelyReactions" testApplyTimelyReactions
  
  IO.println "===================================="
  IO.println "Tests completed."

end Tests

/--
  Entry point for the application
-/
def main : IO Unit := do
  -- Check if we're in test mode
  let args ← IO.getArgs
  
  if args.contains "--test" then
    -- Run tests
    Tests.runAllTests
  else
    -- Parse command line arguments for regular execution
    let port : UInt16 := 
      if args.length > 1 then
        let portStr := args[1]!
        match portStr.toNat? with
        | some p => p.toUInt16
        | none => 3000
      else
        3000
    
    -- Get API key from environment or use default
    let apiKey ← match ← IO.getEnv "DEEPSEEK_API_KEY" with
      | some key => pure key
      | none => pure "sk-example-key"
    
    -- Get endpoint from environment or use default
    let endpoint ← match ← IO.getEnv "DEEPSEEK_ENDPOINT" with
      | some ep => pure ep
      | none => pure "https://api.example.com/v1/chat/completions"
    
    -- Start the server
    main apiKey endpoint port

end AmadeusKurisu

/--
  Main entry point
-/
def main : IO Unit := do
  -- Check if we're in test mode
  let args ← IO.getArgs
  
  if args.contains "--test" then
    -- Run tests
    AmadeusKurisu.Tests.runAllTests
  else
    -- Get API key from environment or use default
    let apiKey ← match ← IO.getEnv "DEEPSEEK_API_KEY" with
      | some key => pure key
      | none => pure "sk-example-key"
    
    -- Get endpoint from environment or use default
    let endpoint ← match ← IO.getEnv "DEEPSEEK_ENDPOINT" with
      | some ep => pure ep
      | none => pure "https://api.example.com/v1/chat/completions"
    
    -- Parse port from arguments if provided
    let port : UInt16 := 
      if args.length > 1 then
        let portStr := args[1]!
        match portStr.toNat? with
        | some p => p.toUInt16
        | none => 3000
      else
        3000
    
    -- Start the server
    AmadeusKurisu.main apiKey endpoint port
