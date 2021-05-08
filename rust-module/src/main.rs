fn main() {
    //println!("Hello, world!");

    let context = zmq::Context::new();

    let publisher = context.socket(zmq::PUB).unwrap();
    publisher.bind("tcp://*:5555").unwrap();
   
    let stdin = std::io::stdin();

    loop {
        let mut line = String::new();
        stdin.read_line(&mut line).unwrap();

        // Say what topic I want to publish on
        publisher.send("TOPIC", zmq::SNDMORE).unwrap();
        // Publish the actual message
        publisher.send(&line, 0).unwrap();
    }
}
