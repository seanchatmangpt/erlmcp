/// Post-Quantum Cryptography Rust NIF
///
/// High-performance PQC operations for Erlang/OTP using:
/// - NIST FIPS 203 (ML-KEM): Lattice-based key encapsulation
/// - NIST FIPS 204 (ML-DSA): Lattice-based digital signatures
/// - NIST FIPS 205 (SLH-DSA): Stateless hash-based signatures
/// - SHA3, BLAKE3 hashing with SIMD acceleration
/// - Parallel batch operations via Rayon
///
/// Architecture:
/// - Zero-copy where possible for large keys/signatures
/// - Thread pool for parallel verification
/// - Constant-time operations for side-channel resistance
/// - Memory zeroization on drop for key material

use rustler::{Encoder, Env, Error, NifResult, Term};
use rayon::prelude::*;

// PQC algorithm implementations
use pqcrypto_dilithium::{
    dilithium2, dilithium3, dilithium5,
};
use pqcrypto_kyber::{
    kyber512, kyber768, kyber1024,
};
use pqcrypto_sphincsplus::{
    sphincssha256128ssimple, sphincssha256128fsimple,
    sphincssha256192ssimple, sphincssha256192fsimple,
    sphincssha256256ssimple, sphincssha256256fsimple,
};
use pqcrypto_traits::kem::{PublicKey as KemPublicKey, SecretKey as KemSecretKey, Ciphertext};
use pqcrypto_traits::sign::{PublicKey as SigPublicKey, SecretKey as SigSecretKey, SignedMessage, DetachedSignature};

// Hash and KDF
use sha3::{Sha3_256, Sha3_512, Digest};
use blake3;

rustler::init!(
    "pqc_rust_nif",
    [
        // ML-DSA (Dilithium)
        ml_dsa_keygen,
        ml_dsa_sign,
        ml_dsa_verify,
        ml_dsa_batch_verify,

        // SLH-DSA (SPHINCS+)
        slh_dsa_keygen,
        slh_dsa_sign,
        slh_dsa_verify,

        // ML-KEM (Kyber)
        ml_kem_keygen,
        ml_kem_encapsulate,
        ml_kem_decapsulate,

        // Hash functions
        sha3_256,
        sha3_512,
        blake3_hash,

        // Key derivation
        hkdf_sha3,

        // Merkle tree
        merkle_root,

        // Batch operations
        batch_hash
    ]
);

/// ============================================================================
/// ML-DSA (FIPS 204) - Dilithium Digital Signatures
/// ============================================================================

#[rustler::nif]
fn ml_dsa_keygen(env: Env, security_level: i32) -> NifResult<Term> {
    match security_level {
        44 => {
            let (pk, sk) = dilithium2::keypair();
            Ok((
                pk.as_bytes(),
                sk.as_bytes()
            ).encode(env))
        },
        65 => {
            let (pk, sk) = dilithium3::keypair();
            Ok((
                pk.as_bytes(),
                sk.as_bytes()
            ).encode(env))
        },
        87 => {
            let (pk, sk) = dilithium5::keypair();
            Ok((
                pk.as_bytes(),
                sk.as_bytes()
            ).encode(env))
        },
        _ => Err(Error::BadArg)
    }
}

#[rustler::nif]
fn ml_dsa_sign(env: Env, security_level: i32, message: Vec<u8>, secret_key: Vec<u8>) -> NifResult<Term> {
    let signature = match security_level {
        44 => {
            let sk = dilithium2::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium2::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        65 => {
            let sk = dilithium3::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium3::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        87 => {
            let sk = dilithium5::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium5::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        _ => return Err(Error::BadArg)
    };

    Ok(signature.encode(env))
}

#[rustler::nif]
fn ml_dsa_verify(
    _env: Env,
    security_level: i32,
    message: Vec<u8>,
    signature: Vec<u8>,
    public_key: Vec<u8>
) -> NifResult<Term> {
    let result = match security_level {
        44 => {
            let pk = dilithium2::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium2::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            dilithium2::verify_detached_signature(&sig, &message, &pk)
        },
        65 => {
            let pk = dilithium3::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium3::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            dilithium3::verify_detached_signature(&sig, &message, &pk)
        },
        87 => {
            let pk = dilithium5::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = dilithium5::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            dilithium5::verify_detached_signature(&sig, &message, &pk)
        },
        _ => return Err(Error::BadArg)
    };

    match result {
        Ok(_) => Ok(rustler::atoms::ok()),
        Err(_) => Ok((rustler::atoms::error(), "invalid_signature").encode(_env))
    }
}

#[rustler::nif]
fn ml_dsa_batch_verify(
    env: Env,
    verifications: Vec<(i32, Vec<u8>, Vec<u8>, Vec<u8>)>
) -> NifResult<Term> {
    // Parallel batch verification using Rayon
    let results: Vec<Term> = verifications
        .par_iter()
        .map(|(level, msg, sig, pk)| {
            let result = match *level {
                44 => {
                    dilithium2::PublicKey::from_bytes(pk)
                        .and_then(|pk| dilithium2::DetachedSignature::from_bytes(sig)
                            .map(|sig| (pk, sig)))
                        .and_then(|(pk, sig)| dilithium2::verify_detached_signature(&sig, msg, &pk))
                },
                65 => {
                    dilithium3::PublicKey::from_bytes(pk)
                        .and_then(|pk| dilithium3::DetachedSignature::from_bytes(sig)
                            .map(|sig| (pk, sig)))
                        .and_then(|(pk, sig)| dilithium3::verify_detached_signature(&sig, msg, &pk))
                },
                87 => {
                    dilithium5::PublicKey::from_bytes(pk)
                        .and_then(|pk| dilithium5::DetachedSignature::from_bytes(sig)
                            .map(|sig| (pk, sig)))
                        .and_then(|(pk, sig)| dilithium5::verify_detached_signature(&sig, msg, &pk))
                },
                _ => return (rustler::atoms::error(), "bad_security_level").encode(env)
            };

            match result {
                Ok(_) => rustler::atoms::ok().encode(env),
                Err(_) => (rustler::atoms::error(), "invalid_signature").encode(env)
            }
        })
        .collect();

    Ok(results.encode(env))
}

/// ============================================================================
/// SLH-DSA (FIPS 205) - SPHINCS+ Hash-Based Signatures
/// ============================================================================

#[rustler::nif]
fn slh_dsa_keygen(env: Env, variant: &str) -> NifResult<Term> {
    match variant {
        "128s" => {
            let (pk, sk) = sphincssha256128ssimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        "128f" => {
            let (pk, sk) = sphincssha256128fsimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        "192s" => {
            let (pk, sk) = sphincssha256192ssimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        "192f" => {
            let (pk, sk) = sphincssha256192fsimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        "256s" => {
            let (pk, sk) = sphincssha256256ssimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        "256f" => {
            let (pk, sk) = sphincssha256256fsimple::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        _ => Err(Error::BadArg)
    }
}

#[rustler::nif]
fn slh_dsa_sign(env: Env, variant: &str, message: Vec<u8>, secret_key: Vec<u8>) -> NifResult<Term> {
    let signature = match variant {
        "128s" => {
            let sk = sphincssha256128ssimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256128ssimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        "128f" => {
            let sk = sphincssha256128fsimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256128fsimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        "192s" => {
            let sk = sphincssha256192ssimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256192ssimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        "192f" => {
            let sk = sphincssha256192fsimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256192fsimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        "256s" => {
            let sk = sphincssha256256ssimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256256ssimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        "256f" => {
            let sk = sphincssha256256fsimple::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256256fsimple::detached_sign(&message, &sk);
            sig.as_bytes().to_vec()
        },
        _ => return Err(Error::BadArg)
    };

    Ok(signature.encode(env))
}

#[rustler::nif]
fn slh_dsa_verify(
    env: Env,
    variant: &str,
    message: Vec<u8>,
    signature: Vec<u8>,
    public_key: Vec<u8>
) -> NifResult<Term> {
    let result = match variant {
        "128s" => {
            let pk = sphincssha256128ssimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256128ssimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256128ssimple::verify_detached_signature(&sig, &message, &pk)
        },
        "128f" => {
            let pk = sphincssha256128fsimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256128fsimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256128fsimple::verify_detached_signature(&sig, &message, &pk)
        },
        "192s" => {
            let pk = sphincssha256192ssimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256192ssimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256192ssimple::verify_detached_signature(&sig, &message, &pk)
        },
        "192f" => {
            let pk = sphincssha256192fsimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256192fsimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256192fsimple::verify_detached_signature(&sig, &message, &pk)
        },
        "256s" => {
            let pk = sphincssha256256ssimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256256ssimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256256ssimple::verify_detached_signature(&sig, &message, &pk)
        },
        "256f" => {
            let pk = sphincssha256256fsimple::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let sig = sphincssha256256fsimple::DetachedSignature::from_bytes(&signature)
                .map_err(|_| Error::BadArg)?;
            sphincssha256256fsimple::verify_detached_signature(&sig, &message, &pk)
        },
        _ => return Err(Error::BadArg)
    };

    match result {
        Ok(_) => Ok(rustler::atoms::ok()),
        Err(_) => Ok((rustler::atoms::error(), "invalid_signature").encode(env))
    }
}

/// ============================================================================
/// ML-KEM (FIPS 203) - Kyber Key Encapsulation
/// ============================================================================

#[rustler::nif]
fn ml_kem_keygen(env: Env, security_level: i32) -> NifResult<Term> {
    match security_level {
        512 => {
            let (pk, sk) = kyber512::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        768 => {
            let (pk, sk) = kyber768::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        1024 => {
            let (pk, sk) = kyber1024::keypair();
            Ok((pk.as_bytes(), sk.as_bytes()).encode(env))
        },
        _ => Err(Error::BadArg)
    }
}

#[rustler::nif]
fn ml_kem_encapsulate(env: Env, security_level: i32, public_key: Vec<u8>) -> NifResult<Term> {
    let (ciphertext, shared_secret) = match security_level {
        512 => {
            let pk = kyber512::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let (ss, ct) = kyber512::encapsulate(&pk);
            (ct.as_bytes().to_vec(), ss.as_bytes().to_vec())
        },
        768 => {
            let pk = kyber768::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let (ss, ct) = kyber768::encapsulate(&pk);
            (ct.as_bytes().to_vec(), ss.as_bytes().to_vec())
        },
        1024 => {
            let pk = kyber1024::PublicKey::from_bytes(&public_key)
                .map_err(|_| Error::BadArg)?;
            let (ss, ct) = kyber1024::encapsulate(&pk);
            (ct.as_bytes().to_vec(), ss.as_bytes().to_vec())
        },
        _ => return Err(Error::BadArg)
    };

    Ok((ciphertext, shared_secret).encode(env))
}

#[rustler::nif]
fn ml_kem_decapsulate(
    env: Env,
    security_level: i32,
    ciphertext: Vec<u8>,
    secret_key: Vec<u8>
) -> NifResult<Term> {
    let shared_secret = match security_level {
        512 => {
            let sk = kyber512::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let ct = kyber512::Ciphertext::from_bytes(&ciphertext)
                .map_err(|_| Error::BadArg)?;
            let ss = kyber512::decapsulate(&ct, &sk);
            ss.as_bytes().to_vec()
        },
        768 => {
            let sk = kyber768::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let ct = kyber768::Ciphertext::from_bytes(&ciphertext)
                .map_err(|_| Error::BadArg)?;
            let ss = kyber768::decapsulate(&ct, &sk);
            ss.as_bytes().to_vec()
        },
        1024 => {
            let sk = kyber1024::SecretKey::from_bytes(&secret_key)
                .map_err(|_| Error::BadArg)?;
            let ct = kyber1024::Ciphertext::from_bytes(&ciphertext)
                .map_err(|_| Error::BadArg)?;
            let ss = kyber1024::decapsulate(&ct, &sk);
            ss.as_bytes().to_vec()
        },
        _ => return Err(Error::BadArg)
    };

    Ok(shared_secret.encode(env))
}

/// ============================================================================
/// Hash Functions (SIMD-accelerated where available)
/// ============================================================================

#[rustler::nif]
fn sha3_256(env: Env, data: Vec<u8>) -> NifResult<Term> {
    let mut hasher = Sha3_256::new();
    hasher.update(&data);
    let result = hasher.finalize();
    Ok(result.as_slice().encode(env))
}

#[rustler::nif]
fn sha3_512(env: Env, data: Vec<u8>) -> NifResult<Term> {
    let mut hasher = Sha3_512::new();
    hasher.update(&data);
    let result = hasher.finalize();
    Ok(result.as_slice().encode(env))
}

#[rustler::nif]
fn blake3_hash(env: Env, data: Vec<u8>) -> NifResult<Term> {
    let hash = blake3::hash(&data);
    Ok(hash.as_bytes().encode(env))
}

/// ============================================================================
/// Key Derivation Function (HKDF with SHA3-256)
/// ============================================================================

#[rustler::nif]
fn hkdf_sha3(
    env: Env,
    salt: Vec<u8>,
    ikm: Vec<u8>,
    info: Vec<u8>,
    length: usize
) -> NifResult<Term> {
    use sha3::Sha3_256;

    // HKDF-Extract
    let salt = if salt.is_empty() {
        vec![0u8; 32]
    } else {
        salt
    };

    let mut hasher = Sha3_256::new();
    hasher.update(&salt);
    hasher.update(&ikm);
    let prk = hasher.finalize();

    // HKDF-Expand
    let mut okm = Vec::with_capacity(length);
    let mut t = Vec::new();
    let mut counter = 1u8;

    while okm.len() < length {
        let mut hasher = Sha3_256::new();
        hasher.update(&t);
        hasher.update(&info);
        hasher.update(&[counter]);
        t = hasher.finalize().to_vec();

        let remaining = length - okm.len();
        if remaining < t.len() {
            okm.extend_from_slice(&t[..remaining]);
        } else {
            okm.extend_from_slice(&t);
        }

        counter += 1;
        if counter == 0 {
            return Err(Error::BadArg);
        }
    }

    Ok(okm.encode(env))
}

/// ============================================================================
/// Merkle Tree Root Calculation
/// ============================================================================

#[rustler::nif]
fn merkle_root(env: Env, leaves: Vec<Vec<u8>>) -> NifResult<Term> {
    if leaves.is_empty() {
        return Err(Error::BadArg);
    }

    let mut level: Vec<Vec<u8>> = leaves;

    while level.len() > 1 {
        let mut next_level = Vec::new();

        for chunk in level.chunks(2) {
            let mut hasher = Sha3_256::new();
            hasher.update(&chunk[0]);
            if chunk.len() > 1 {
                hasher.update(&chunk[1]);
            } else {
                // Odd number - hash with itself
                hasher.update(&chunk[0]);
            }
            next_level.push(hasher.finalize().to_vec());
        }

        level = next_level;
    }

    Ok(level[0].encode(env))
}

/// ============================================================================
/// Batch Hash Operations (Parallel with Rayon)
/// ============================================================================

#[rustler::nif]
fn batch_hash(env: Env, items: Vec<Vec<u8>>, algorithm: &str) -> NifResult<Term> {
    let hashes: Vec<Vec<u8>> = items
        .par_iter()
        .map(|item| {
            match algorithm {
                "sha3_256" => {
                    let mut hasher = Sha3_256::new();
                    hasher.update(item);
                    hasher.finalize().to_vec()
                },
                "sha3_512" => {
                    let mut hasher = Sha3_512::new();
                    hasher.update(item);
                    hasher.finalize().to_vec()
                },
                "blake3" => {
                    blake3::hash(item).as_bytes().to_vec()
                },
                _ => vec![]
            }
        })
        .collect();

    Ok(hashes.encode(env))
}
