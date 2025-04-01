// 2-persistent/main.rs
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;
use serde::Serialize;

#[derive(Serialize)]
struct FileResponse {
    file: String,
    contents: String,
}

fn handle_client(mut stream: TcpStream) {
    let peer = stream.peer_addr().unwrap();
    println!("Client connected: {peer}");

    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let mut line = String::new();

    while let Ok(n) = reader.read_line(&mut line) {
        if n == 0 {
            // client disconnected
            break;
        }

        let request = line.trim_end();
        println!("Received request: {request}");

        let response = match fs::read_to_string(request) {
            Ok(contents) => {
                let reversed_lines: String = contents
                    .lines()
                    .rev()
                    .collect::<Vec<_>>()
                    .join("\n");
                FileResponse {
                    file: request.to_string(),
                    contents: reversed_lines,
                }
            }
            Err(e) => FileResponse {
                file: request.to_string(),
                contents: format!("Error reading file: {e}"),
            },
        };

        let json = serde_json::to_string(&response).unwrap();
        writeln!(stream, "{json}").unwrap();
        stream.flush().unwrap();

        line.clear();
    }

    println!("Client disconnected: {peer}");
}

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:1729")?;
    println!("Listening on port 1729...");

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(move || handle_client(stream));
            }
            Err(e) => {
                eprintln!("Connection failed: {e}");
            }
        }
    }

    Ok(())
}
