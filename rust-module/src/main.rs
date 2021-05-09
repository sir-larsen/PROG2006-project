fn gameloop() {
    //Subscriber
    let context_sub = zmq::Context::new();
    let subscriber = context_sub.socket(zmq::SUB).unwrap();
    subscriber.connect("tcp://127.0.0.1:5000").unwrap();
    subscriber.set_subscribe("".as_ref());

    //Publisher
    let context_pub = zmq::Context::new();
    let publisher = context_pub.socket(zmq::PUB).unwrap();
    publisher.bind("tcp://*:5555").unwrap();
    let stdin = std::io::stdin();

    loop {
        let mut msg = zmq::Message::new(); //Message that is to be received
        subscriber.recv(&mut msg, 0).unwrap();
        println!("{}", msg.as_str().unwrap()); //Printing the received message

        let words:Vec<&str>= msg.as_str().unwrap().split(' ').collect(); //Splitting the message into tokens
        if words[0].contains("Please") { //If so, then go to input

            let mut line = String::new(); 
            stdin.read_line(&mut line).unwrap();
            publisher.send(&line, 0).unwrap();
        }
        if words[3].contains("winner") { //If winner, quit game
            println!("WINNER, GAME OVER NOW");
        }
        if words[0].contains("draw") { //If draw, quit game also
            println!("DRAW GAME OVAH");
        }
    }
}

fn main() {
    gameloop();
}
