// Code from Hansen and Rischel: Functional Programming using F#     1       6/12 2012
// Chapter 13: Asynchronous and parallel computations.          Revised MRH 25/11 2013 
// Code from Section 13.5: 13.5 Reactive programs.

#load "NimAi.fs"

open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing

open NimAi

// An asynchronous event queue kindly provided by Don Syme 
type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() = 
        match queue.Count, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)

// The window part
let window =
  new Form(Text="Nim", Size=Size(525,825))

let ansBox =
  new TextBox(Location=Point(150,150),Size=Size(200,25))

let sizeBox =
  new TextBox(Location=Point(150,250),Size=Size(200,25))

let gameBox =
  new TextBox(Location=Point(200,350),Size=Size(100,25), Text="")

let x =
  new TextBox(Location=Point(150,425),Size=Size(100,50), Text="")

let SetupButton =
  new Button(Location=Point(200,275),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Setup Game")

let DownloadDataButton = 
    new Button(Location=Point(150,10),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Download Game Data")

let cancelButton =
  new Button(Location=Point(250,10),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="CANCEL")

let urlBox =
  new TextBox(Location=Point(150,70),Size=Size(200,25))

let userButton =
  new Button(Location=Point(150,375),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="User move")

let AIButton =
  new Button(Location=Point(250,375),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Ai Move")

let toggle bs = 
    for b in [userButton;AIButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false
let enable bs = 
    for b in [userButton;AIButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- true


// An enumeration of the possible events 
type Message =
  | Start of string | Board of string | Clear | Cancel 
  | Reset of string | Error | Cancelled | Game | AI of string 
  | User of string | Web of string | Download of string

//exception UnexpectedMessage

// The dialogue automaton 
let ev = AsyncEventQueue()
let defaulturl = "http://www2.compute.dtu.dk/~mire/nim.game"

let rec ready() = 
  async {ansBox.Text <- "Waiting for input."
         urlBox.Text <- defaulturl
         //sizeBox.Text <- "1 2 3"
         toggle [userButton;AIButton;cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Download url  -> return! getGameData(url)
         | Board s       -> return! draw((SetFromString s))
         | AI s          -> return! AI_move((SetFromString s))
         | _             -> failwith("ready: unexpected message")}

and draw(l) =
  async {ansBox.Text <- "Game ready"
         gameBox.Text <- (StringFromSet l)
         SetupButton.Text <- "Reset Game"
         enable [userButton;AIButton;DownloadDataButton]
         //toggle [SetupButton]
         let! msg = ev.Receive()
         match msg with
         | Download url  -> return! getGameData(url)
         | AI s -> return! AI_move(AiMove l)
         | User s -> let xy = SetFromString s
                     let yx = List.rev (SetFromString s)
                     return! User_move((UserMove (xy.Head) (yx.Head) l))
         | Board s -> return! draw((SetFromString s))
         | _    ->  failwith("draw_error")}

and AI_move(l) =
  async {ansBox.Text <- "AI Move"
         if (calcXor l)=0 then ansBox.Text <- "Haha i win"
         gameBox.Text <- (StringFromSet l)
         toggle [AIButton;DownloadDataButton]
         //enable [quitButton;AIButton;userButton]
         let! msg = ev.Receive()
         printfn"%A" msg
         match msg with
         | AI s -> return! AI_move((AiMove l))
         | User s -> let xy = SetFromString s
                     let yx = List.rev (SetFromString s)
                     return! User_move((UserMove (xy.Head) (yx.Head) l))
         | Board s -> return! draw((SetFromString s))
         | _    ->  failwith("AI_move Error")}

 and User_move(l) =
  async {ansBox.Text <- "User Move"
         gameBox.Text <- (StringFromSet l)
         toggle [userButton;DownloadDataButton]
         //disable [ fetchButton; clearButton; cancelButton;quitButton;AIButton;userButton]
         //enable [quitButton;AIButton;userButton]
         let! msg = ev.Receive()
         printfn"%A" msg
         match msg with
         | AI s -> return! AI_move((AiMove l))
         | User s -> let xy = SetFromString s
                     let yx = List.rev (SetFromString s)
                     return! User_move((UserMove (xy.Head) (yx.Head) l))
         | Board s -> return! draw((SetFromString s))
         | _    ->  failwith("AI_move Error")}

 and getGameData(url) = 
    async { ansBox.Text <- "Downloading"
            SetupButton.Text <- "Setup game"
            use ts = new CancellationTokenSource()
            enable [cancelButton]
            //do! Async.Sleep (2 * 1000)
            // start the load
            Async.StartWithContinuations
                    (async { let webCl = new WebClient()
                             let! html = webCl.AsyncDownloadString(Uri url)
                             return html },
                     (fun html -> ev.Post (Web html)),
                     (fun _ -> ev.Post Error),
                     (fun _ -> ev.Post Cancelled),
                     ts.Token)
                  
            let! msg = ev.Receive()
            match msg with
            | Web html ->
                let ans = String.Format("{0:D}",html)
                sizeBox.Text <- ans
                //printfn "%A" ans
                return! ready()
            | Error   -> return! ready()
            | Cancel  -> ts.Cancel()
                         return! cancelling()
            | _       -> failwith("loading: unexpected message")}

and cancelling() =
  async {ansBox.Text <- "Cancelling"
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! ready()
         | _    ->  failwith("cancelling: unexpected message")}


// Initialization
window.Controls.Add ansBox
window.Controls.Add sizeBox
window.Controls.Add gameBox
window.Controls.Add x
window.Controls.Add urlBox
window.Controls.Add SetupButton
window.Controls.Add userButton
window.Controls.Add AIButton
window.Controls.Add DownloadDataButton
window.Controls.Add cancelButton
SetupButton.Click.Add (fun _ -> ev.Post (Board sizeBox.Text))
userButton.Click.Add (fun _ -> ev.Post (User x.Text))
AIButton.Click.Add (fun _ -> ev.Post (AI gameBox.Text))
DownloadDataButton.Click.Add (fun _ -> ev.Post (Download urlBox.Text))
cancelButton.Click.Add (fun _ -> ev.Post Cancel)

// Start
Async.StartImmediate (ready())
window.Show()