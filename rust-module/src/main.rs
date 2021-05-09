//fn print_board() {
    
    //Publisher
//    let context = zmq::Context::new();
//    let publisher = context.socket(zmq::PUB).unwrap();
//    publisher.bind("tcp://*:5555").unwrap();
//    let stdin = std::io::stdin();

//    loop {
//        let mut payload = zmq::Message::new()

//    }
//}

fn print_board(/*board : &str*/) {
    print!("| | | | | | |\n| | | | | | |\n| | | | | |\n");
}

fn main() {
    //SUBSCRIBER
    let context = zmq::Context::new();
    let subscriber = context.socket(zmq::SUB).unwrap();

    subscriber.connect("tcp://127.0.0.1:5000").unwrap();
    subscriber.set_subscribe("".as_ref());

    let stdin = std::io::stdin();

    let mut msg = zmq::Message::new();

    loop {
        //let mut msg = zmq::Message::new();
        subscriber.recv(&mut msg, 0).unwrap();
        //subscriber.recv(&mut msg, 0).unwrap();
        println!("{:?}", msg.as_str().unwrap())
    }
    //SUBSCRIBER 2
    // Create the socket
    /*let sub_socket = context.socket(zmq::SUB)?;
    // Subscribe the a topic
    sub_socket.set_subscribe(b"A")?;
    // Connect to the server
    sub_socket.connect("tcp://127.0.0.1:5000")?;

    let mut msg = zmq::Message::new();

    loop {
        // Receive the envelope frame. This gets overritten by the next recv(). There is probably a more efficient way of doing this.
        sub_socket.recv(&mut msg, 0).unwrap();
        // Reveice the actual message
        sub_socket.recv(&mut msg, 0).unwrap();
        println!("Received: {:?}", msg.as_str());
    }*/

    ////////

    //println!("Hello, world!");

    //let context = zmq::Context::new();

    //let publisher = context.socket(zmq::PUB).unwrap();
    //publisher.bind("tcp://*:5555").unwrap();
   
    //let stdin = std::io::stdin();

    /*loop {
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();

        if line == "cock" {
            break;
        }
        // Say what topic I want to publish on
        //publisher.send("TOPIC", zmq::SNDMORE).unwrap();
        // Publish the actual message
        publisher.send(&line, 0).unwrap();
    }*/
    //println!("fuck!")
    //print_board();
}
