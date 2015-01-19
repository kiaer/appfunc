module Game

open System
open System.Windows.Forms
open System.Drawing
open System.Net 
open System.Threading 

open GUI
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
         if (calcXor l)=0 && List.sum l >0 then ansBox.Text <- "Haha i win :)"
         gameBox.Text <- (StringFromSet l)
         toggle [AIButton]
         if List.sum l = 0 then toggle[AIButton;userButton]
                                gameBox.Text<-"I win win win" 
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
         
         if List.sum l = 0 then toggle[AIButton;userButton]
                                gameBox.Text<-"Aw man you win"
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