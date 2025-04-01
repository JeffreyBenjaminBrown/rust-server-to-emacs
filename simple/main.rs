use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

fn handle_client(mut stream: TcpStream) {
    let peer = stream.peer_addr().unwrap();
    println!("Client connected: {peer}");

    let mut reader = BufReader::new(stream.try_clone().unwrap());

    let mut msg = String::new();
    if reader.read_line(&mut msg).unwrap() > 0 {
        let trimmed = msg.trim_end();
        println!("Received: {trimmed}");
        writeln!(stream, "Echo: {trimmed}").unwrap();
        stream.flush().unwrap();  // Ensure response is sent
    }

    println!("Client disconnected: {peer}");
    // Connection closes automatically when `stream` is dropped
}

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:1729")?;
    println!("Listening on port 1729...");
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                std::thread::spawn(move || handle_client(stream));
            }
            Err(e) => {
                eprintln!("Connection failed: {e}");
            }
        }
    }
    Ok(())
}
