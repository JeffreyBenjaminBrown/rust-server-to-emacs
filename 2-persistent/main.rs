// 2-persistent/main.rs
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;
use serde::{Serialize,Deserialize};

// What Rust receives from Emacs.
#[derive(Deserialize)]
struct FileRequest { action: String,
		     path: String, }

// What Rust sends in response.
#[derive(Serialize)]
struct FileResponse { file: String,
		      contents: String, }

fn main() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:1729")?;
    println!("Listening on port 1729...");
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(move ||
			      handle_emacs(stream)); }
            Err(e) => {
                eprintln!("Connection failed: {e}");
            } } }
    Ok(()) }

fn handle_emacs(mut stream: TcpStream) {
    let peer = stream.peer_addr().unwrap();
    println!("Emacs connected: {peer}");
    let mut reader =
	BufReader::new(stream.try_clone().unwrap());
    let mut line = String::new();
    while let Ok(n) = reader.read_line(&mut line) {
        if n == 0 { break; } // emacs disconnected
        if let Some(response) = process_request(&line) {
            send_response(&mut stream, &response); }
        line.clear(); }
    println!("Emacs disconnected: {peer}"); }

fn process_request(line: &str) -> Option<FileResponse> {
    let trimmed = line.trim_end();
    println!("Received raw: {trimmed}");
    match serde_json::from_str::<FileRequest>(trimmed) {
        Ok(FileRequest { action, path })
	    if action == "get-file" => {
            Some(read_file_and_build_response(&path)) }
        Ok(req) => Some(FileResponse {
            file: String::new(),
            contents: format!(
		"Unsupported action: {}", req.action), }),
        Err(e) => Some(FileResponse {
            file: String::new(),
            contents: format!(
		"Invalid JSON request: {e}"), } ), } }

fn read_file_and_build_response(path: &str)
				-> FileResponse {
    match fs::read_to_string(path) {
        Ok(contents) => {
            let reversed_lines = contents
                .lines()
                .rev()
                .collect::<Vec<_>>()
                .join("\n");
            FileResponse {
                file: path.to_string(),
                contents: reversed_lines, } }
        Err(e) => FileResponse {
            file: path.to_string(),
            contents: format!("Error reading file: {e}"),
        }, } }

fn send_response(stream: &mut TcpStream,
		 response: &FileResponse) {
    let json = serde_json::to_string(response).unwrap();
    writeln!(stream, "{json}").unwrap();
    stream.flush().unwrap(); }
