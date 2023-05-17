//-*-Mode:rust;coding:utf-8;tab-width:2;c-basic-offset:2;indent-tabs-mode:()-*-
//ex: set ft=rust fenc=utf-8 sts=2 ts=2 sw=2 et nomod:

// MIT License
//
// Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#![crate_name = "erlang_rs"]
#![crate_type = "lib"]

use std::collections::BTreeMap;

// tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
const TAG_VERSION: u8 = 131;
const TAG_COMPRESSED_ZLIB: u8 = 80;
const TAG_NEW_FLOAT_EXT: u8 = 70;
const TAG_BIT_BINARY_EXT: u8 = 77;
const TAG_ATOM_CACHE_REF: u8 = 78;
const TAG_NEW_PID_EXT: u8 = 88;
const TAG_NEW_PORT_EXT: u8 = 89;
const TAG_NEWER_REFERENCE_EXT: u8 = 90;
const TAG_SMALL_INTEGER_EXT: u8 = 97;
const TAG_INTEGER_EXT: u8 = 98;
const TAG_FLOAT_EXT: u8 = 99;
const TAG_ATOM_EXT: u8 = 100;
const TAG_REFERENCE_EXT: u8 = 101;
const TAG_PORT_EXT: u8 = 102;
const TAG_PID_EXT: u8 = 103;
const TAG_SMALL_TUPLE_EXT: u8 = 104;
const TAG_LARGE_TUPLE_EXT: u8 = 105;
const TAG_NIL_EXT: u8 = 106;
const TAG_STRING_EXT: u8 = 107;
const TAG_LIST_EXT: u8 = 108;
const TAG_BINARY_EXT: u8 = 109;
const TAG_SMALL_BIG_EXT: u8 = 110;
const TAG_LARGE_BIG_EXT: u8 = 111;
const TAG_NEW_FUN_EXT: u8 = 112;
const TAG_EXPORT_EXT: u8 = 113;
const TAG_NEW_REFERENCE_EXT: u8 = 114;
const TAG_SMALL_ATOM_EXT: u8 = 115;
const TAG_MAP_EXT: u8 = 116;
const TAG_FUN_EXT: u8 = 117;
const TAG_ATOM_UTF8_EXT: u8 = 118;
const TAG_SMALL_ATOM_UTF8_EXT: u8 = 119;
const TAG_V4_PORT_EXT: u8 = 120;
const TAG_LOCAL_EXT: u8 = 121;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Float {
    bits: u64,
}

impl Float {
    pub fn value(&self) -> f64 {
        f64::from_bits(self.bits)
    }
}

impl From<f64> for Float {
    fn from(x: f64) -> Self {
        Float {
            bits: x.to_bits(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Pid {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    serial: Vec<u8>,
    creation: Vec<u8>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Port {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    creation: Vec<u8>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Reference {
    node_tag: u8,
    node: Vec<u8>,
    id: Vec<u8>,
    creation: Vec<u8>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Function {
    tag: u8,
    value: Vec<u8>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum OtpErlangTerm {
    OtpErlangInteger(i32),
    OtpErlangFloat(Float),
    OtpErlangAtom(Vec<u8>),
    OtpErlangAtomUTF8(Vec<u8>),
    OtpErlangAtomCacheRef(u8),
    OtpErlangAtomBool(bool),
    OtpErlangString(Vec<u8>),
    OtpErlangBinary(Vec<u8>),
    OtpErlangBinaryBits(Vec<u8>, u8),
    OtpErlangList(Vec<OtpErlangTerm>),
    OtpErlangListImproper(Vec<OtpErlangTerm>),
    OtpErlangTuple(Vec<OtpErlangTerm>),
    OtpErlangMap(BTreeMap<OtpErlangTerm, OtpErlangTerm>),
    OtpErlangPid(Pid),
    OtpErlangPort(Port),
    OtpErlangReference(Reference),
    OtpErlangFunction(Function),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
    InputError(&'static str),
    OutputError(&'static str),
    ParseError(&'static str),
    UnexpectedError(),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    source: Option<Box<dyn std::error::Error + Send + Sync>>,
}

impl Error {
    fn new<E>(error: E) -> Self
    where E: Into<Box<dyn std::error::Error + Send + Sync>>, {
        Error {
            kind: ErrorKind::UnexpectedError(),
            source: Some(error.into()),
        }
    }
}

impl PartialEq<Error> for Error {
    fn eq(&self, other: &Error) -> bool {
        self.kind == (*other).kind
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.source {
            None => None,
            Some(e) => Some(&**e)
        }
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error {
            kind,
            source: None,
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(error: std::num::ParseFloatError) -> Self {
        Error::new(error)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

fn slice_get<I>(data: &[u8], index: I) ->
Result<&<I as std::slice::SliceIndex<[u8]>>::Output>
where I: std::slice::SliceIndex<[u8]>, {
    match data.get(index) {
        Some(result) => Ok(result),
        None => Err(ErrorKind::ParseError("missing data").into()),
    }
}

fn unpack_u16(i: &mut usize, data: &[u8]) -> Result<u16> {
    let byte0 = *slice_get(data, *i)?;
    let byte1 = *slice_get(data, *i + 1)?;
    *i += 2;
    Ok(
        ((byte0 as u16) << 8) |
        (byte1 as u16)
    )
}

fn unpack_u32(i: &mut usize, data: &[u8]) -> Result<u32> {
    let byte0 = *slice_get(data, *i)?;
    let byte1 = *slice_get(data, *i + 1)?;
    let byte2 = *slice_get(data, *i + 2)?;
    let byte3 = *slice_get(data, *i + 3)?;
    *i += 4;
    Ok(
        ((byte0 as u32) << 24) |
        ((byte1 as u32) << 16) |
        ((byte2 as u32) << 8) |
        (byte3 as u32)
    )
}

fn unpack_f64(i: &mut usize, data: &[u8]) -> Result<f64> {
    let byte0 = *slice_get(data, *i)?;
    let byte1 = *slice_get(data, *i + 1)?;
    let byte2 = *slice_get(data, *i + 2)?;
    let byte3 = *slice_get(data, *i + 3)?;
    let byte4 = *slice_get(data, *i + 4)?;
    let byte5 = *slice_get(data, *i + 5)?;
    let byte6 = *slice_get(data, *i + 6)?;
    let byte7 = *slice_get(data, *i + 7)?;
    *i += 8;
    Ok(f64::from_bits(
        ((byte0 as u64) << 56) |
        ((byte1 as u64) << 48) |
        ((byte2 as u64) << 40) |
        ((byte3 as u64) << 32) |
        ((byte4 as u64) << 24) |
        ((byte5 as u64) << 16) |
        ((byte6 as u64) << 8) |
        (byte7 as u64)
    ))
}

pub fn binary_to_term(data: &[u8]) -> Result<OtpErlangTerm> {
    let size = data.len();
    if size <= 1 {
        return Err(ErrorKind::ParseError("null input").into());
    }
    if data[0] != TAG_VERSION {
        return Err(ErrorKind::ParseError("invalid version").into());
    }
    let mut i: usize = 1;
    let term = binary_to_term_(&mut i, data)?;
    if i == size {
        Ok(term)
    }
    else {
        Err(ErrorKind::ParseError("unparsed data").into())
    }
}

pub fn term_to_binary(_term: OtpErlangTerm) -> Result<Vec<u8>> {
    //XXX
    return Err(ErrorKind::ParseError("not impl").into());
}

fn binary_to_term_(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_NEW_FLOAT_EXT => {
            let float = unpack_f64(i, data)?;
            Ok(OtpErlangTerm::OtpErlangFloat(float.into()))
        },
        TAG_BIT_BINARY_EXT => {
            let j = unpack_u32(i, data)? as usize;
            let bits = *slice_get(data, *i)?;
            *i += 1;
            let binary = slice_get(data, *i..*i + j)?;
            *i += j;
            if bits == 8 {
                Ok(OtpErlangTerm::OtpErlangBinary(binary.to_vec()))
            }
            else {
                Ok(OtpErlangTerm::OtpErlangBinaryBits(binary.to_vec(), bits))
            }
        },
        TAG_ATOM_CACHE_REF => {
            let atom = *slice_get(data, *i)?;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangAtomCacheRef(atom))
        },
        TAG_SMALL_INTEGER_EXT => {
            let integer = *slice_get(data, *i)? as i32;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_INTEGER_EXT => {
            let integer = unpack_u32(i, data)? as i32;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_FLOAT_EXT => {
            let float_bytes = slice_get(data, *i..*i + 31)?;
            *i += 31;
            let float_str = unsafe {
                std::str::from_utf8_unchecked(float_bytes)
            };
            let float = float_str.parse::<f64>()?;
            Ok(OtpErlangTerm::OtpErlangFloat(float.into()))
        },
        TAG_V4_PORT_EXT |
        TAG_NEW_PORT_EXT |
        TAG_REFERENCE_EXT |
        TAG_PORT_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id_size = match tag {
                TAG_V4_PORT_EXT => 8,
                _ => 4,
            };
            let id = slice_get(data, *i..*i + id_size)?;
            *i += id_size;
            let creation_size = match tag {
                TAG_V4_PORT_EXT | TAG_NEW_PORT_EXT => 4,
                _ => 1,
            };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            if tag == TAG_REFERENCE_EXT {
                Ok(OtpErlangTerm::OtpErlangReference(Reference {
                    node_tag,
                    node,
                    id: id.to_vec(),
                    creation: creation.to_vec(),
                }))
            }
            else {
                Ok(OtpErlangTerm::OtpErlangPort(Port {
                    node_tag,
                    node,
                    id: id.to_vec(),
                    creation: creation.to_vec(),
                }))
            }
        },
        TAG_NEW_PID_EXT |
        TAG_PID_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let serial = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let creation_size = if tag == TAG_NEW_PID_EXT { 4 } else { 1 };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            Ok(OtpErlangTerm::OtpErlangPid(Pid {
                node_tag,
                node,
                id: id.to_vec(),
                serial: serial.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        TAG_SMALL_TUPLE_EXT => {
            let length = *slice_get(data, *i)? as usize;
            *i += 1;
            let tmp = binary_to_term_sequence(i, length, data)?;
            Ok(OtpErlangTerm::OtpErlangTuple(tmp))
        },
        TAG_LARGE_TUPLE_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let tmp = binary_to_term_sequence(i, length, data)?;
            Ok(OtpErlangTerm::OtpErlangTuple(tmp))
        },
        TAG_NIL_EXT => {
            Ok(OtpErlangTerm::OtpErlangList(Vec::new()))
        },
        TAG_STRING_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let string = slice_get(data, *i..*i + j)?;
            *i += j;
            Ok(OtpErlangTerm::OtpErlangString(string.to_vec()))
        },
        TAG_LIST_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let mut tmp = binary_to_term_sequence(i, length, data)?;
            match binary_to_term_(i, data)? {
                OtpErlangTerm::OtpErlangList(v) if v.is_empty() => {
                    Ok(OtpErlangTerm::OtpErlangList(tmp))
                },
                tail => {
                    tmp.push(tail);
                    Ok(OtpErlangTerm::OtpErlangListImproper(tmp))
                },
            }
        },
        TAG_BINARY_EXT => {
            let j = unpack_u32(i, data)? as usize;
            let binary = slice_get(data, *i..*i + j)?;
            *i += j;
            Ok(OtpErlangTerm::OtpErlangBinary(binary.to_vec()))
        },
        TAG_SMALL_BIG_EXT |
        TAG_LARGE_BIG_EXT => {
            Err(ErrorKind::ParseError("rust doesn't provide bigint").into())
        },
        TAG_NEW_FUN_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let value = slice_get(data, *i..*i + length)?;
            *i += length;
            Ok(OtpErlangTerm::OtpErlangFunction(Function {
                tag,
                value: value.to_vec(),
            }))
        },
        TAG_EXPORT_EXT => {
            let i0 = *i;
            let _ = binary_to_atom(i, data)?;
            let _ = binary_to_atom(i, data)?;
            if *slice_get(data, *i)? != TAG_SMALL_INTEGER_EXT {
                Err(ErrorKind::ParseError("invalid small integer tag").into())
            }
            else {
                *i += 2;
                let value = slice_get(data, i0..*i)?;
                Ok(OtpErlangTerm::OtpErlangFunction(Function {
                    tag,
                    value: value.to_vec(),
                }))
            }
        },
        TAG_NEWER_REFERENCE_EXT |
        TAG_NEW_REFERENCE_EXT => {
            let j = (unpack_u16(i, data)? as usize) * 4;
            let (node_tag, node) = binary_to_atom(i, data)?;
            let creation_size = match tag {
                TAG_NEWER_REFERENCE_EXT => 4,
                _ => 1,
            };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            let id = slice_get(data, *i..*i + j)?;
            Ok(OtpErlangTerm::OtpErlangReference(Reference {
                node_tag,
                node,
                id: id.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        TAG_MAP_EXT => {
            let length = unpack_u32(i, data)? as usize;
            let mut pairs = BTreeMap::new();
            for _ in 0..length {
                let key = binary_to_term_(i, data)?;
                let value = binary_to_term_(i, data)?;
                pairs.insert(key, value);
            }
            Ok(OtpErlangTerm::OtpErlangMap(pairs))
        },
        TAG_FUN_EXT => {
            let i0 = *i;
            let numfree = unpack_u32(i, data)? as usize;
            let _ = binary_to_pid(i, data)?; // pid
            let _ = binary_to_atom(i, data)?; // module
            let _ = binary_to_integer(i, data)?; // index
            let _ = binary_to_integer(i, data)?; // uniq
            let _ = binary_to_term_sequence(i, numfree, data)?; // free
            let value = slice_get(data, i0..*i)?;
            Ok(OtpErlangTerm::OtpErlangFunction(Function {
                tag,
                value: value.to_vec(),
            }))
        },
        TAG_ATOM_UTF8_EXT |
        TAG_ATOM_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let atom_name = slice_get(data, *i..*i + j)?;
            *i += j;
            match atom_name {
                b"true" => Ok(OtpErlangTerm::OtpErlangAtomBool(true)),
                b"false" => Ok(OtpErlangTerm::OtpErlangAtomBool(false)),
                _ if tag == TAG_ATOM_UTF8_EXT => {
                    Ok(OtpErlangTerm::OtpErlangAtomUTF8(atom_name.to_vec()))
                },
                _ => Ok(OtpErlangTerm::OtpErlangAtom(atom_name.to_vec())),
            }
        },
        TAG_SMALL_ATOM_UTF8_EXT |
        TAG_SMALL_ATOM_EXT => {
            let j = *slice_get(data, *i)? as usize;
            *i += 1;
            let atom_name = slice_get(data, *i..*i + j)?;
            *i += j;
            match atom_name {
                b"true" => Ok(OtpErlangTerm::OtpErlangAtomBool(true)),
                b"false" => Ok(OtpErlangTerm::OtpErlangAtomBool(false)),
                _ if tag == TAG_SMALL_ATOM_UTF8_EXT => {
                    Ok(OtpErlangTerm::OtpErlangAtomUTF8(atom_name.to_vec()))
                },
                _ => Ok(OtpErlangTerm::OtpErlangAtom(atom_name.to_vec())),
            }
        },
        TAG_COMPRESSED_ZLIB => {
            Err(ErrorKind::ParseError("rust doesn't provide zlib").into())
        },
        TAG_LOCAL_EXT => {
            Err(ErrorKind::ParseError("LOCAL_EXT is opaque").into())
        },
        _ => Err(ErrorKind::ParseError("invalid tag").into()),
    }
}

fn binary_to_term_sequence(i: &mut usize, length: usize, data: &[u8]) ->
Result<Vec<OtpErlangTerm>> {
    let mut sequence: Vec<OtpErlangTerm> = Vec::new();
    for _ in 0..length {
        sequence.push(binary_to_term_(i, data)?);
    }
    Ok(sequence)
}

fn binary_to_integer(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_SMALL_INTEGER_EXT => {
            let integer = *slice_get(data, *i)? as i32;
            *i += 1;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        TAG_INTEGER_EXT => {
            let integer = unpack_u32(i, data)? as i32;
            Ok(OtpErlangTerm::OtpErlangInteger(integer))
        },
        _ => Err(ErrorKind::ParseError("invalid integer tag").into()),
    }
}

fn binary_to_pid(i: &mut usize, data: &[u8]) -> Result<OtpErlangTerm> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_NEW_PID_EXT |
        TAG_PID_EXT => {
            let (node_tag, node) = binary_to_atom(i, data)?;
            let id = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let serial = slice_get(data, *i..*i + 4)?;
            *i += 4;
            let creation_size = if tag == TAG_NEW_PID_EXT { 4 } else { 1 };
            let creation = slice_get(data, *i..*i + creation_size)?;
            *i += creation_size;
            Ok(OtpErlangTerm::OtpErlangPid(Pid {
                node_tag,
                node,
                id: id.to_vec(),
                serial: serial.to_vec(),
                creation: creation.to_vec(),
            }))
        },
        _ => Err(ErrorKind::ParseError("invalid pid tag").into()),
    }
}

fn binary_to_atom(i: &mut usize, data: &[u8]) -> Result<(u8, Vec<u8>)> {
    let tag = *slice_get(data, *i)?;
    *i += 1;
    match tag {
        TAG_ATOM_CACHE_REF => {
            let value = slice_get(data, *i..*i + 1)?;
            *i += 1;
            Ok((tag, value.to_vec()))
        },
        TAG_ATOM_UTF8_EXT |
        TAG_ATOM_EXT => {
            let j = unpack_u16(i, data)? as usize;
            let value = slice_get(data, *i - 2..*i + j)?;
            *i += j;
            Ok((tag, value.to_vec()))
        },
        TAG_SMALL_ATOM_UTF8_EXT |
        TAG_SMALL_ATOM_EXT => {
            let j = *slice_get(data, *i)? as usize;
            let value = slice_get(data, *i..*i + 1 + j)?;
            *i += 1 + j;
            Ok((tag, value.to_vec()))
        },
        _ => Err(ErrorKind::ParseError("invalid atom tag").into()),
    }
}

// MIT LICENSE (of tests below)
//
// Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>
// Copyright (c) 2009-2013 Dmitry Vasiliev <dima@hlabs.org>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

#[cfg(test)]
mod tests {
    use crate::*;

    //XXX add test_* type functions

    #[test]
    fn test_decode_basic() {
        assert_eq!(binary_to_term(b"").unwrap_err(),
                   ErrorKind::ParseError("null input").into());
        assert_eq!(binary_to_term(b"\x83").unwrap_err(),
                   ErrorKind::ParseError("null input").into());
        assert_eq!(binary_to_term(b"\x83z").unwrap_err(),
                   ErrorKind::ParseError("invalid tag").into());
    }

    #[test]
    fn test_decode_atom() {
        assert_eq!(binary_to_term(b"\x83d").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83d\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83s\x00").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83d\x00\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"test".to_vec()));
        assert_eq!(binary_to_term(b"\x83s\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"test".to_vec()));
    }

    #[test]
    fn test_decode_predefined_atom() {
        assert_eq!(binary_to_term(b"\x83s\x04true").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(true));
        assert_eq!(binary_to_term(b"\x83s\x05false").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(false));
        assert_eq!(binary_to_term(b"\x83s\x09undefined").unwrap(),
                   OtpErlangTerm::OtpErlangAtom(b"undefined".to_vec()));
        assert_eq!(binary_to_term(b"\x83w\x04true").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(true));
        assert_eq!(binary_to_term(b"\x83w\x05false").unwrap(),
                   OtpErlangTerm::OtpErlangAtomBool(false));
        assert_eq!(binary_to_term(b"\x83w\x09undefined").unwrap(),
                   OtpErlangTerm::OtpErlangAtomUTF8(b"undefined".to_vec()));
    }

    #[test]
    fn test_decode_empty_list() {
        assert_eq!(binary_to_term(b"\x83j").unwrap(),
                   OtpErlangTerm::OtpErlangList(Vec::new()));
    }

    #[test]
    fn test_decode_string_list() {
        assert_eq!(binary_to_term(b"\x83k").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83k\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangString(b"".to_vec()));
        assert_eq!(binary_to_term(b"\x83k\x00\x04test").unwrap(),
                   OtpErlangTerm::OtpErlangString(b"test".to_vec()));
    }

    #[test]
    fn test_decode_list() {
        assert_eq!(binary_to_term(b"\x83l").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00j").unwrap(),
                   OtpErlangTerm::OtpErlangList(Vec::new()));
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x02jjj").unwrap(),
                   OtpErlangTerm::OtpErlangList(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_improper_list() {
        assert_eq!(binary_to_term(b"\x83l\x00\x00\x00\x00k").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83l\x00\x00\x00\x01jd\x00\x04tail").unwrap(),
            OtpErlangTerm::OtpErlangListImproper(vec![
                OtpErlangTerm::OtpErlangList(Vec::new()),
                OtpErlangTerm::OtpErlangAtom(b"tail".to_vec()),
            ]));
    }

    #[test]
    fn test_decode_small_tuple() {
        assert_eq!(binary_to_term(b"\x83h").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83h\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83h\x00").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(Vec::new()));
        assert_eq!(binary_to_term(b"\x83h\x02jj").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_large_tuple() {
        assert_eq!(binary_to_term(b"\x83i").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(Vec::new()));
        assert_eq!(binary_to_term(b"\x83i\x00\x00\x00\x02jj").unwrap(),
                   OtpErlangTerm::OtpErlangTuple(vec![
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                       OtpErlangTerm::OtpErlangList(Vec::new()),
                   ]));
    }

    #[test]
    fn test_decode_small_integer() {
        assert_eq!(binary_to_term(b"\x83a").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83a\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(0));
        assert_eq!(binary_to_term(b"\x83a\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(255));
    }

    #[test]
    fn test_decode_integer() {
        assert_eq!(binary_to_term(b"\x83b").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83b\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(0));
        assert_eq!(binary_to_term(b"\x83b\x7f\xff\xff\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(i32::MAX));
        assert_eq!(binary_to_term(b"\x83b\x80\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(i32::MIN));
        assert_eq!(binary_to_term(b"\x83b\xff\xff\xff\xff").unwrap(),
                   OtpErlangTerm::OtpErlangInteger(-1));
    }

    #[test]
    fn test_decode_binary() {
        assert_eq!(binary_to_term(b"\x83m").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x01").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x00").unwrap(),
                   OtpErlangTerm::OtpErlangBinary(Vec::new()));
        assert_eq!(binary_to_term(b"\x83m\x00\x00\x00\x04data").unwrap(),
                   OtpErlangTerm::OtpErlangBinary(b"data".to_vec()));
    }

    #[test]
    fn test_decode_float() {
        assert_eq!(binary_to_term(b"\x83F").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(binary_to_term(b"\x83F\x00\x00\x00\x00\x00").unwrap_err(),
                   ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00").unwrap_err(),
            ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00\x00").unwrap_err(),
            ErrorKind::ParseError("missing data").into());
        assert_eq!(
            binary_to_term(b"\x83F\x00\x00\x00\x00\x00\x00\x00\x00").unwrap(),
            OtpErlangTerm::OtpErlangFloat(0.0.into()));
        assert_eq!(
            binary_to_term(b"\x83F?\xf8\x00\x00\x00\x00\x00\x00").unwrap(),
            OtpErlangTerm::OtpErlangFloat(1.5.into()));
    }

    //XXX add test_encode_* term_to_binary functions
}

