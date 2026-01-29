//! MCP+ Governance Computational Core
//!
//! Non-networked computational functions for the governance framework:
//! - Cryptographic hashing (SHA-256)
//! - Ed25519 signatures
//! - Canonical JSON encoding
//! - Evidence bundle serialization (MCPB format)
//! - Merkle tree computation
//!
//! All functions are exposed as Erlang NIFs via rustler.

use base64::Engine;
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use rustler::{Atom, Binary, Encoder, Env, NifResult, OwnedBinary, Term};
use serde_json::{Map, Value};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        invalid_json,
        invalid_key,
        invalid_signature,
        verification_failed,
    }
}

rustler::init!("mcp_governance_nif");

// ============================================================================
// Cryptographic Hashing
// ============================================================================

/// Compute SHA-256 hash of binary data.
/// Returns a 32-byte binary.
#[rustler::nif]
fn sha256_hash(data: Binary) -> OwnedBinary {
    let hash = Sha256::digest(data.as_slice());
    let mut result = OwnedBinary::new(32).unwrap();
    result.as_mut_slice().copy_from_slice(&hash);
    result
}

/// Compute SHA-256 hash of JSON data with canonical encoding.
/// Maps are sorted by key before hashing for deterministic output.
#[rustler::nif]
fn sha256_hash_json(json_binary: Binary) -> NifResult<OwnedBinary> {
    let json_str =
        std::str::from_utf8(json_binary.as_slice()).map_err(|_| rustler::Error::BadArg)?;

    let value: Value = serde_json::from_str(json_str).map_err(|_| rustler::Error::BadArg)?;

    let canonical = canonical_json(&value);
    let canonical_str = serde_json::to_string(&canonical).map_err(|_| rustler::Error::BadArg)?;

    let hash = Sha256::digest(canonical_str.as_bytes());
    let mut result = OwnedBinary::new(32).unwrap();
    result.as_mut_slice().copy_from_slice(&hash);
    Ok(result)
}

// ============================================================================
// Ed25519 Signatures
// ============================================================================

/// Generate an Ed25519 keypair.
/// Returns {ok, {PublicKey, PrivateKey}} where both are 32-byte binaries.
#[rustler::nif]
fn ed25519_generate_keypair(env: Env) -> Term {
    let signing_key = SigningKey::generate(&mut OsRng);
    let verifying_key = signing_key.verifying_key();

    let mut public_key = OwnedBinary::new(32).unwrap();
    public_key
        .as_mut_slice()
        .copy_from_slice(verifying_key.as_bytes());

    let mut private_key = OwnedBinary::new(32).unwrap();
    private_key
        .as_mut_slice()
        .copy_from_slice(signing_key.as_bytes());

    (
        atoms::ok(),
        (public_key.release(env), private_key.release(env)),
    )
        .encode(env)
}

/// Sign data with an Ed25519 private key.
/// Returns {ok, Signature} where Signature is a 64-byte binary.
#[rustler::nif]
fn ed25519_sign<'a>(env: Env<'a>, private_key: Binary, data: Binary) -> NifResult<Term<'a>> {
    if private_key.len() != 32 {
        return Ok((atoms::error(), atoms::invalid_key()).encode(env));
    }

    let key_bytes: [u8; 32] = private_key
        .as_slice()
        .try_into()
        .map_err(|_| rustler::Error::BadArg)?;
    let signing_key = SigningKey::from_bytes(&key_bytes);

    let signature = signing_key.sign(data.as_slice());

    let mut sig_binary = OwnedBinary::new(64).unwrap();
    sig_binary
        .as_mut_slice()
        .copy_from_slice(&signature.to_bytes());

    Ok((atoms::ok(), sig_binary.release(env)).encode(env))
}

/// Verify an Ed25519 signature.
/// Returns ok | {error, verification_failed | invalid_key | invalid_signature}.
#[rustler::nif]
fn ed25519_verify<'a>(
    env: Env<'a>,
    public_key: Binary,
    signature: Binary,
    data: Binary,
) -> Term<'a> {
    if public_key.len() != 32 {
        return (atoms::error(), atoms::invalid_key()).encode(env);
    }
    if signature.len() != 64 {
        return (atoms::error(), atoms::invalid_signature()).encode(env);
    }

    let key_bytes: [u8; 32] = match public_key.as_slice().try_into() {
        Ok(b) => b,
        Err(_) => return (atoms::error(), atoms::invalid_key()).encode(env),
    };

    let sig_bytes: [u8; 64] = match signature.as_slice().try_into() {
        Ok(b) => b,
        Err(_) => return (atoms::error(), atoms::invalid_signature()).encode(env),
    };

    let verifying_key = match VerifyingKey::from_bytes(&key_bytes) {
        Ok(k) => k,
        Err(_) => return (atoms::error(), atoms::invalid_key()).encode(env),
    };

    let sig = match Signature::from_bytes(&sig_bytes) {
        s => s,
    };

    match verifying_key.verify(data.as_slice(), &sig) {
        Ok(()) => atoms::ok().encode(env),
        Err(_) => (atoms::error(), atoms::verification_failed()).encode(env),
    }
}

// ============================================================================
// Canonical JSON Encoding
// ============================================================================

/// Encode a value to canonical JSON (sorted keys for maps).
/// Input: JSON binary
/// Output: {ok, CanonicalJsonBinary} | {error, invalid_json}
#[rustler::nif]
fn canonical_json_encode<'a>(env: Env<'a>, json_binary: Binary) -> NifResult<Term<'a>> {
    let json_str =
        std::str::from_utf8(json_binary.as_slice()).map_err(|_| rustler::Error::BadArg)?;

    let value: Value = match serde_json::from_str(json_str) {
        Ok(v) => v,
        Err(_) => return Ok((atoms::error(), atoms::invalid_json()).encode(env)),
    };

    let canonical = canonical_json(&value);
    let canonical_str = match serde_json::to_string(&canonical) {
        Ok(s) => s,
        Err(_) => return Ok((atoms::error(), atoms::invalid_json()).encode(env)),
    };

    let mut result = OwnedBinary::new(canonical_str.len()).unwrap();
    result
        .as_mut_slice()
        .copy_from_slice(canonical_str.as_bytes());

    Ok((atoms::ok(), result.release(env)).encode(env))
}

/// Convert JSON value to canonical form (sorted keys).
fn canonical_json(value: &Value) -> Value {
    match value {
        Value::Object(map) => {
            let sorted: BTreeMap<String, Value> = map
                .iter()
                .map(|(k, v)| (k.clone(), canonical_json(v)))
                .collect();
            // Convert BTreeMap to Map maintaining order
            let canonical_map: Map<String, Value> =
                sorted.into_iter().collect();
            Value::Object(canonical_map)
        }
        Value::Array(arr) => Value::Array(arr.iter().map(canonical_json).collect()),
        other => other.clone(),
    }
}

// ============================================================================
// Evidence Bundle Serialization (MCPB format)
// ============================================================================

/// MCPB binary format:
/// - Magic: "MCPB" (4 bytes)
/// - Version: "1.0" + null (4 bytes)
/// - Timestamp: i64 milliseconds (8 bytes)
/// - Contract hash: 32 bytes
/// - Merkle root: 32 bytes
/// - Receipt count: u32 (4 bytes)
/// - Receipts data length: u32 (4 bytes)
/// - Receipts data: variable
/// - Signature: 64 bytes

const MCPB_MAGIC: &[u8] = b"MCPB";
const MCPB_VERSION: &[u8] = b"1.0\0";

/// Serialize an evidence bundle to MCPB format.
/// Input: map with keys: timestamp, contract_hash, merkle_root, receipts_json, private_key
/// Output: {ok, Binary} | {error, Reason}
#[rustler::nif]
fn mcpb_serialize<'a>(
    env: Env<'a>,
    timestamp: i64,
    contract_hash: Binary,
    merkle_root: Binary,
    receipts_json: Binary,
    private_key: Binary,
) -> NifResult<Term<'a>> {
    if contract_hash.len() != 32 || merkle_root.len() != 32 || private_key.len() != 32 {
        return Ok((atoms::error(), atoms::invalid_key()).encode(env));
    }

    let receipts_data = receipts_json.as_slice();
    let receipt_count = count_json_array_elements(receipts_data).unwrap_or(0);

    // Build unsigned data
    let unsigned_len = 4 + 4 + 8 + 32 + 32 + 4 + 4 + receipts_data.len();
    let mut unsigned_data = Vec::with_capacity(unsigned_len);

    unsigned_data.extend_from_slice(MCPB_MAGIC);
    unsigned_data.extend_from_slice(MCPB_VERSION);
    unsigned_data.extend_from_slice(&timestamp.to_be_bytes());
    unsigned_data.extend_from_slice(contract_hash.as_slice());
    unsigned_data.extend_from_slice(merkle_root.as_slice());
    unsigned_data.extend_from_slice(&(receipt_count as u32).to_be_bytes());
    unsigned_data.extend_from_slice(&(receipts_data.len() as u32).to_be_bytes());
    unsigned_data.extend_from_slice(receipts_data);

    // Sign the data
    let key_bytes: [u8; 32] = private_key
        .as_slice()
        .try_into()
        .map_err(|_| rustler::Error::BadArg)?;
    let signing_key = SigningKey::from_bytes(&key_bytes);
    let signature = signing_key.sign(&unsigned_data);

    // Append signature
    let mut result = OwnedBinary::new(unsigned_len + 64).unwrap();
    result.as_mut_slice()[..unsigned_len].copy_from_slice(&unsigned_data);
    result.as_mut_slice()[unsigned_len..].copy_from_slice(&signature.to_bytes());

    Ok((atoms::ok(), result.release(env)).encode(env))
}

/// Parse MCPB binary and verify signature.
/// Returns {ok, #{timestamp, contract_hash, merkle_root, receipts_json}} | {error, Reason}
#[rustler::nif]
fn mcpb_parse<'a>(env: Env<'a>, data: Binary, public_key: Binary) -> NifResult<Term<'a>> {
    let data_slice = data.as_slice();

    // Minimum size: 4+4+8+32+32+4+4+64 = 152 bytes
    if data_slice.len() < 152 {
        return Ok((atoms::error(), "too_short").encode(env));
    }

    // Verify magic
    if &data_slice[0..4] != MCPB_MAGIC {
        return Ok((atoms::error(), "invalid_magic").encode(env));
    }

    // Verify version
    if &data_slice[4..8] != MCPB_VERSION {
        return Ok((atoms::error(), "invalid_version").encode(env));
    }

    // Parse header
    let timestamp = i64::from_be_bytes(data_slice[8..16].try_into().unwrap());
    let contract_hash = &data_slice[16..48];
    let merkle_root = &data_slice[48..80];
    let _receipt_count = u32::from_be_bytes(data_slice[80..84].try_into().unwrap());
    let receipts_len = u32::from_be_bytes(data_slice[84..88].try_into().unwrap()) as usize;

    // Verify data length
    let expected_len = 88 + receipts_len + 64;
    if data_slice.len() != expected_len {
        return Ok((atoms::error(), "invalid_length").encode(env));
    }

    let receipts_json = &data_slice[88..88 + receipts_len];
    let signature = &data_slice[88 + receipts_len..];

    // Verify signature
    if public_key.len() != 32 {
        return Ok((atoms::error(), atoms::invalid_key()).encode(env));
    }

    let key_bytes: [u8; 32] = public_key
        .as_slice()
        .try_into()
        .map_err(|_| rustler::Error::BadArg)?;
    let sig_bytes: [u8; 64] = signature.try_into().map_err(|_| rustler::Error::BadArg)?;

    let verifying_key = match VerifyingKey::from_bytes(&key_bytes) {
        Ok(k) => k,
        Err(_) => return Ok((atoms::error(), atoms::invalid_key()).encode(env)),
    };

    let sig = Signature::from_bytes(&sig_bytes);
    let unsigned_data = &data_slice[..88 + receipts_len];

    if verifying_key.verify(unsigned_data, &sig).is_err() {
        return Ok((atoms::error(), atoms::verification_failed()).encode(env));
    }

    // Build result
    let mut contract_hash_bin = OwnedBinary::new(32).unwrap();
    contract_hash_bin
        .as_mut_slice()
        .copy_from_slice(contract_hash);

    let mut merkle_root_bin = OwnedBinary::new(32).unwrap();
    merkle_root_bin
        .as_mut_slice()
        .copy_from_slice(merkle_root);

    let mut receipts_bin = OwnedBinary::new(receipts_len).unwrap();
    receipts_bin.as_mut_slice().copy_from_slice(receipts_json);

    // Return as tuple for simplicity (Erlang will convert to map)
    Ok((
        atoms::ok(),
        (
            timestamp,
            contract_hash_bin.release(env),
            merkle_root_bin.release(env),
            receipts_bin.release(env),
        ),
    )
        .encode(env))
}

/// Count elements in a JSON array.
fn count_json_array_elements(data: &[u8]) -> Option<usize> {
    let json_str = std::str::from_utf8(data).ok()?;
    let value: Value = serde_json::from_str(json_str).ok()?;
    match value {
        Value::Array(arr) => Some(arr.len()),
        _ => None,
    }
}

// ============================================================================
// Merkle Tree Computation
// ============================================================================

/// Compute Merkle root from a list of hashes.
/// Input: list of 32-byte hash binaries
/// Output: 32-byte Merkle root binary
#[rustler::nif]
fn merkle_root(hashes: Vec<Binary>) -> OwnedBinary {
    if hashes.is_empty() {
        // Empty tree: hash of empty string
        let root = Sha256::digest(b"");
        let mut result = OwnedBinary::new(32).unwrap();
        result.as_mut_slice().copy_from_slice(&root);
        return result;
    }

    // Convert to Vec<[u8; 32]>
    let mut leaves: Vec<[u8; 32]> = hashes
        .iter()
        .filter_map(|h| {
            if h.len() == 32 {
                let arr: [u8; 32] = h.as_slice().try_into().ok()?;
                Some(arr)
            } else {
                None
            }
        })
        .collect();

    if leaves.is_empty() {
        let root = Sha256::digest(b"");
        let mut result = OwnedBinary::new(32).unwrap();
        result.as_mut_slice().copy_from_slice(&root);
        return result;
    }

    // Pad to power of 2 if needed
    let target_len = leaves.len().next_power_of_two();
    while leaves.len() < target_len {
        leaves.push(leaves[leaves.len() - 1]);
    }

    // Build tree bottom-up
    while leaves.len() > 1 {
        let mut new_level = Vec::with_capacity(leaves.len() / 2);
        for chunk in leaves.chunks(2) {
            let mut hasher = Sha256::new();
            hasher.update(&chunk[0]);
            hasher.update(&chunk[1]);
            let hash: [u8; 32] = hasher.finalize().into();
            new_level.push(hash);
        }
        leaves = new_level;
    }

    let mut result = OwnedBinary::new(32).unwrap();
    result.as_mut_slice().copy_from_slice(&leaves[0]);
    result
}

/// Verify a Merkle proof.
/// Input: leaf_hash, proof (list of sibling hashes), root, index
/// Output: true | false
#[rustler::nif]
fn merkle_verify(leaf: Binary, proof: Vec<Binary>, root: Binary, index: u64) -> bool {
    if leaf.len() != 32 || root.len() != 32 {
        return false;
    }

    let leaf_arr: [u8; 32] = match leaf.as_slice().try_into() {
        Ok(a) => a,
        Err(_) => return false,
    };

    let root_arr: [u8; 32] = match root.as_slice().try_into() {
        Ok(a) => a,
        Err(_) => return false,
    };

    let mut current = leaf_arr;
    let mut idx = index;

    for sibling in proof.iter() {
        if sibling.len() != 32 {
            return false;
        }
        let sibling_arr: [u8; 32] = match sibling.as_slice().try_into() {
            Ok(a) => a,
            Err(_) => return false,
        };

        let mut hasher = Sha256::new();
        if idx % 2 == 0 {
            hasher.update(&current);
            hasher.update(&sibling_arr);
        } else {
            hasher.update(&sibling_arr);
            hasher.update(&current);
        }
        current = hasher.finalize().into();
        idx /= 2;
    }

    current == root_arr
}

// ============================================================================
// Base64 Encoding
// ============================================================================

/// Encode binary to base64.
#[rustler::nif]
fn base64_encode(data: Binary) -> OwnedBinary {
    let encoded = base64::engine::general_purpose::STANDARD.encode(data.as_slice());
    let mut result = OwnedBinary::new(encoded.len()).unwrap();
    result.as_mut_slice().copy_from_slice(encoded.as_bytes());
    result
}

/// Decode base64 to binary.
#[rustler::nif]
fn base64_decode<'a>(env: Env<'a>, data: Binary) -> Term<'a> {
    let input = match std::str::from_utf8(data.as_slice()) {
        Ok(s) => s,
        Err(_) => return (atoms::error(), "invalid_utf8").encode(env),
    };

    match base64::engine::general_purpose::STANDARD.decode(input) {
        Ok(decoded) => {
            let mut result = OwnedBinary::new(decoded.len()).unwrap();
            result.as_mut_slice().copy_from_slice(&decoded);
            (atoms::ok(), result.release(env)).encode(env)
        }
        Err(_) => (atoms::error(), "invalid_base64").encode(env),
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_canonical_json() {
        let input = r#"{"b": 2, "a": 1}"#;
        let value: Value = serde_json::from_str(input).unwrap();
        let canonical = canonical_json(&value);
        let output = serde_json::to_string(&canonical).unwrap();
        assert_eq!(output, r#"{"a":1,"b":2}"#);
    }

    #[test]
    fn test_canonical_json_nested() {
        let input = r#"{"z": {"b": 2, "a": 1}, "a": [3, 2, 1]}"#;
        let value: Value = serde_json::from_str(input).unwrap();
        let canonical = canonical_json(&value);
        let output = serde_json::to_string(&canonical).unwrap();
        assert_eq!(output, r#"{"a":[3,2,1],"z":{"a":1,"b":2}}"#);
    }

    #[test]
    fn test_merkle_single() {
        // Single leaf: root = leaf
        let hash = Sha256::digest(b"test");
        let leaves = vec![hash.as_slice()];

        let mut current: [u8; 32] = hash.into();

        // Pad to 2 elements
        let mut padded = vec![current, current];

        // Build tree
        let mut hasher = Sha256::new();
        hasher.update(&padded[0]);
        hasher.update(&padded[1]);
        let root: [u8; 32] = hasher.finalize().into();

        // With single element, it gets duplicated and hashed
        assert_eq!(root.len(), 32);
    }
}
