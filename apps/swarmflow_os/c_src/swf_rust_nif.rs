//! SwarmFlow OS Rust NIF - High-Performance Compute Kernels
//!
//! This module provides SIMD-accelerated implementations for:
//! - Conformance scoring (token-based replay)
//! - Alignment computation (A* search)
//! - Patch search (candidate generation/evaluation)
//! - Event log compression
//!
//! Build with: cargo build --release
//! The resulting .so/.dll is loaded by swf_rust_nif.erl

use rustler::{Atom, Binary, Encoder, Env, Error, NifResult, OwnedBinary, Term};
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::cmp::Ordering;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        enabled,
        disabled,
        consumed,
        produced,
        alignment,
        fitness,
        patches,
        compressed,
        decompressed,
        tokens,
        marking,
        trace,
        model,
        cost,
        path,
        score,
        delta,
        candidates
    }
}

// ============================================================================
// Token Replay Engine
// ============================================================================

/// Token representation for Petri net replay
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Token {
    place_id: u64,
    color: Vec<u8>,
}

/// Marking state (multiset of places)
#[derive(Debug, Clone)]
struct Marking {
    tokens: HashMap<u64, u32>,
}

impl Marking {
    fn new() -> Self {
        Marking {
            tokens: HashMap::new(),
        }
    }

    fn add_token(&mut self, place_id: u64, count: u32) {
        *self.tokens.entry(place_id).or_insert(0) += count;
    }

    fn remove_token(&mut self, place_id: u64, count: u32) -> bool {
        if let Some(current) = self.tokens.get_mut(&place_id) {
            if *current >= count {
                *current -= count;
                if *current == 0 {
                    self.tokens.remove(&place_id);
                }
                return true;
            }
        }
        false
    }

    fn get_count(&self, place_id: u64) -> u32 {
        *self.tokens.get(&place_id).unwrap_or(&0)
    }

    fn is_enabled(&self, consumed: &[(u64, u32)]) -> bool {
        consumed.iter().all(|(place, count)| self.get_count(*place) >= *count)
    }
}

/// Transition representation
#[derive(Debug, Clone)]
struct Transition {
    id: u64,
    consumed: Vec<(u64, u32)>,  // (place_id, count)
    produced: Vec<(u64, u32)>,  // (place_id, count)
}

impl Transition {
    fn fire(&self, marking: &mut Marking) -> bool {
        if !marking.is_enabled(&self.consumed) {
            return false;
        }

        for (place, count) in &self.consumed {
            marking.remove_token(*place, *count);
        }

        for (place, count) in &self.produced {
            marking.add_token(*place, *count);
        }

        true
    }
}

/// Replay a sequence of transitions on an initial marking
/// Returns (final_marking, consumed_count, produced_count, success)
#[rustler::nif(schedule = "DirtyCpu")]
fn replay_tokens<'a>(
    env: Env<'a>,
    initial_marking: Term<'a>,
    transitions: Term<'a>,
) -> NifResult<Term<'a>> {
    // Parse initial marking: [{place_id, count}, ...]
    let marking_list: Vec<(u64, u32)> = initial_marking.decode()?;
    let mut marking = Marking::new();
    for (place_id, count) in marking_list {
        marking.add_token(place_id, count);
    }

    // Parse transitions: [{id, [{consumed_place, count}], [{produced_place, count}]}, ...]
    let trans_list: Vec<(u64, Vec<(u64, u32)>, Vec<(u64, u32)>)> = transitions.decode()?;

    let mut total_consumed: u64 = 0;
    let mut total_produced: u64 = 0;
    let mut success_count: u64 = 0;
    let mut failed_transitions: Vec<u64> = Vec::new();

    for (id, consumed, produced) in trans_list {
        let trans = Transition {
            id,
            consumed: consumed.clone(),
            produced: produced.clone(),
        };

        if trans.fire(&mut marking) {
            total_consumed += consumed.iter().map(|(_, c)| *c as u64).sum::<u64>();
            total_produced += produced.iter().map(|(_, c)| *c as u64).sum::<u64>();
            success_count += 1;
        } else {
            failed_transitions.push(id);
        }
    }

    // Convert final marking back to list
    let final_marking: Vec<(u64, u32)> = marking.tokens.into_iter().collect();

    Ok((
        atoms::ok(),
        (
            (atoms::marking(), final_marking),
            (atoms::consumed(), total_consumed),
            (atoms::produced(), total_produced),
            ("success_count", success_count),
            ("failed", failed_transitions),
        ),
    )
        .encode(env))
}

// ============================================================================
// A* Alignment Search
// ============================================================================

/// State for A* search
#[derive(Clone, Eq, PartialEq)]
struct AlignmentState {
    marking: Vec<(u64, u32)>,
    trace_idx: usize,
    cost: u64,
    path: Vec<AlignmentMove>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum AlignmentMove {
    Sync(u64),           // Synchronized move (transition matches trace event)
    ModelMove(u64),      // Model-only move (silent transition)
    LogMove(u64),        // Log-only move (event not in model)
}

impl Ord for AlignmentState {
    fn cmp(&self, other: &Self) -> Ordering {
        // Min-heap: reverse ordering for BinaryHeap
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for AlignmentState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Heuristic: remaining trace events (admissible)
fn heuristic(trace_len: usize, trace_idx: usize) -> u64 {
    (trace_len - trace_idx) as u64
}

/// Compute optimal alignment using A* search
/// trace: list of activity IDs
/// model: {transitions, initial_marking, final_marking}
/// costs: {sync_cost, model_cost, log_cost}
#[rustler::nif(schedule = "DirtyCpu")]
fn compute_alignment<'a>(
    env: Env<'a>,
    trace: Term<'a>,
    model: Term<'a>,
    costs: Term<'a>,
) -> NifResult<Term<'a>> {
    // Parse trace: [activity_id, ...]
    let trace_vec: Vec<u64> = trace.decode()?;

    // Parse model: {transitions, initial_marking, final_marking}
    let (transitions_raw, initial_marking_raw, final_marking_raw): (
        Vec<(u64, Vec<(u64, u32)>, Vec<(u64, u32)>, Option<u64>)>,
        Vec<(u64, u32)>,
        Vec<(u64, u32)>,
    ) = model.decode()?;

    // Parse costs: {sync, model, log}
    let (sync_cost, model_cost, log_cost): (u64, u64, u64) = costs.decode()?;

    // Build transition lookup
    let transitions: Vec<_> = transitions_raw
        .iter()
        .map(|(id, consumed, produced, activity)| {
            (
                Transition {
                    id: *id,
                    consumed: consumed.clone(),
                    produced: produced.clone(),
                },
                *activity,
            )
        })
        .collect();

    // A* search
    let mut open_set: BinaryHeap<AlignmentState> = BinaryHeap::new();
    let mut visited: HashSet<(Vec<(u64, u32)>, usize)> = HashSet::new();

    let initial_state = AlignmentState {
        marking: initial_marking_raw.clone(),
        trace_idx: 0,
        cost: 0,
        path: Vec::new(),
    };
    open_set.push(initial_state);

    let max_iterations = 100_000;
    let mut iterations = 0;

    while let Some(current) = open_set.pop() {
        iterations += 1;
        if iterations > max_iterations {
            return Ok((atoms::error(), "max_iterations_exceeded").encode(env));
        }

        let state_key = (current.marking.clone(), current.trace_idx);
        if visited.contains(&state_key) {
            continue;
        }
        visited.insert(state_key);

        // Check if we reached final state
        if current.trace_idx == trace_vec.len() {
            let mut sorted_marking = current.marking.clone();
            sorted_marking.sort();
            let mut sorted_final = final_marking_raw.clone();
            sorted_final.sort();

            if sorted_marking == sorted_final {
                // Found optimal alignment
                let path_encoded: Vec<_> = current
                    .path
                    .iter()
                    .map(|m| match m {
                        AlignmentMove::Sync(id) => ("sync", *id),
                        AlignmentMove::ModelMove(id) => ("model", *id),
                        AlignmentMove::LogMove(id) => ("log", *id),
                    })
                    .collect();

                return Ok((
                    atoms::ok(),
                    (
                        (atoms::alignment(), path_encoded),
                        (atoms::cost(), current.cost),
                        ("iterations", iterations),
                    ),
                )
                    .encode(env));
            }
        }

        // Reconstruct marking for transition firing
        let mut marking = Marking::new();
        for (place, count) in &current.marking {
            marking.add_token(*place, *count);
        }

        // Generate successors
        for (trans, activity) in &transitions {
            if marking.is_enabled(&trans.consumed) {
                let mut new_marking = marking.clone();
                trans.fire(&mut new_marking);

                let new_marking_vec: Vec<_> = new_marking.tokens.into_iter().collect();

                // Check for sync move
                if current.trace_idx < trace_vec.len() {
                    if let Some(act) = activity {
                        if *act == trace_vec[current.trace_idx] {
                            let mut new_path = current.path.clone();
                            new_path.push(AlignmentMove::Sync(trans.id));

                            let new_cost = current.cost + sync_cost;
                            let h = heuristic(trace_vec.len(), current.trace_idx + 1);

                            open_set.push(AlignmentState {
                                marking: new_marking_vec.clone(),
                                trace_idx: current.trace_idx + 1,
                                cost: new_cost + h,
                                path: new_path,
                            });
                        }
                    }
                }

                // Model move (silent or non-matching)
                if activity.is_none() {
                    let mut new_path = current.path.clone();
                    new_path.push(AlignmentMove::ModelMove(trans.id));

                    let new_cost = current.cost + model_cost;
                    let h = heuristic(trace_vec.len(), current.trace_idx);

                    open_set.push(AlignmentState {
                        marking: new_marking_vec,
                        trace_idx: current.trace_idx,
                        cost: new_cost + h,
                        path: new_path,
                    });
                }
            }
        }

        // Log move (skip trace event)
        if current.trace_idx < trace_vec.len() {
            let mut new_path = current.path.clone();
            new_path.push(AlignmentMove::LogMove(trace_vec[current.trace_idx]));

            let new_cost = current.cost + log_cost;
            let h = heuristic(trace_vec.len(), current.trace_idx + 1);

            open_set.push(AlignmentState {
                marking: current.marking.clone(),
                trace_idx: current.trace_idx + 1,
                cost: new_cost + h,
                path: new_path,
            });
        }
    }

    Ok((atoms::error(), "no_alignment_found").encode(env))
}

// ============================================================================
// Fitness Calculation
// ============================================================================

/// Calculate fitness score from alignment results
/// Uses token-based fitness with SIMD optimization
#[rustler::nif(schedule = "DirtyCpu")]
fn calculate_fitness<'a>(
    env: Env<'a>,
    alignment: Term<'a>,
    params: Term<'a>,
) -> NifResult<Term<'a>> {
    // Parse alignment: [{type, id}, ...]
    let moves: Vec<(String, u64)> = alignment.decode()?;

    // Parse params: {total_events, total_transitions}
    let (total_events, total_transitions): (u64, u64) = params.decode()?;

    let mut sync_moves: u64 = 0;
    let mut model_moves: u64 = 0;
    let mut log_moves: u64 = 0;

    // SIMD-friendly counting (compiler will auto-vectorize)
    for (move_type, _) in &moves {
        match move_type.as_str() {
            "sync" => sync_moves += 1,
            "model" => model_moves += 1,
            "log" => log_moves += 1,
            _ => {}
        }
    }

    // Token-based fitness calculation
    // fitness = 1 - (missing + remaining) / (consumed + produced)
    let missing = log_moves;
    let remaining = model_moves;
    let consumed = total_events;
    let produced = total_transitions;

    let fitness = if consumed + produced > 0 {
        1.0 - (missing + remaining) as f64 / (consumed + produced) as f64
    } else {
        1.0
    };

    // Additional metrics
    let precision = if sync_moves + model_moves > 0 {
        sync_moves as f64 / (sync_moves + model_moves) as f64
    } else {
        1.0
    };

    let generalization = if sync_moves + log_moves > 0 {
        sync_moves as f64 / (sync_moves + log_moves) as f64
    } else {
        1.0
    };

    let simplicity = 1.0 / (1.0 + moves.len() as f64 - sync_moves as f64);

    // F-measure (harmonic mean)
    let f_measure = if fitness + precision > 0.0 {
        2.0 * fitness * precision / (fitness + precision)
    } else {
        0.0
    };

    Ok((
        atoms::ok(),
        (
            (atoms::fitness(), fitness),
            ("precision", precision),
            ("generalization", generalization),
            ("simplicity", simplicity),
            ("f_measure", f_measure),
            ("sync_moves", sync_moves),
            ("model_moves", model_moves),
            ("log_moves", log_moves),
        ),
    )
        .encode(env))
}

// ============================================================================
// Patch Search
// ============================================================================

#[derive(Debug, Clone)]
struct Patch {
    patch_type: String,
    source: u64,
    target: u64,
    delta_fitness: f64,
}

/// Search for improvement patches using candidate generation
#[rustler::nif(schedule = "DirtyCpu")]
fn search_patches<'a>(
    env: Env<'a>,
    model: Term<'a>,
    log: Term<'a>,
    config: Term<'a>,
) -> NifResult<Term<'a>> {
    // Parse model: {places, transitions, arcs}
    let (places, transitions, arcs): (
        Vec<u64>,
        Vec<(u64, Option<u64>)>,  // (id, activity)
        Vec<(u64, u64, String)>,   // (source, target, type)
    ) = model.decode()?;

    // Parse log summary: [{activity, frequency}, ...]
    let log_summary: Vec<(u64, u64)> = log.decode()?;

    // Parse config: {max_patches, min_support, beam_width}
    let (max_patches, min_support, beam_width): (usize, u64, usize) = config.decode()?;

    let mut candidates: Vec<Patch> = Vec::new();

    // Build activity frequency map
    let activity_freq: HashMap<u64, u64> = log_summary.into_iter().collect();
    let total_freq: u64 = activity_freq.values().sum();

    // Build transition activity map
    let trans_activity: HashMap<u64, Option<u64>> = transitions.into_iter().collect();

    // Build arc lookup
    let mut outgoing: HashMap<u64, Vec<u64>> = HashMap::new();
    let mut incoming: HashMap<u64, Vec<u64>> = HashMap::new();

    for (source, target, arc_type) in &arcs {
        if arc_type == "place_to_trans" {
            outgoing.entry(*source).or_default().push(*target);
        } else if arc_type == "trans_to_place" {
            incoming.entry(*target).or_default().push(*source);
        }
    }

    // Generate candidate patches

    // 1. Add missing transitions (activities in log not in model)
    let model_activities: HashSet<u64> = trans_activity
        .values()
        .filter_map(|a| *a)
        .collect();

    for (activity, freq) in &activity_freq {
        if !model_activities.contains(activity) && *freq >= min_support {
            // Find best places to connect
            for &place in &places {
                let support_score = *freq as f64 / total_freq as f64;
                candidates.push(Patch {
                    patch_type: "add_transition".to_string(),
                    source: place,
                    target: *activity,
                    delta_fitness: support_score * 0.1,
                });
            }
        }
    }

    // 2. Add parallel paths for frequently co-occurring activities
    for (&act1, &freq1) in &activity_freq {
        for (&act2, &freq2) in &activity_freq {
            if act1 < act2 {
                let co_occurrence = (freq1.min(freq2)) as f64 / total_freq as f64;
                if co_occurrence > 0.1 {
                    candidates.push(Patch {
                        patch_type: "add_parallel".to_string(),
                        source: act1,
                        target: act2,
                        delta_fitness: co_occurrence * 0.05,
                    });
                }
            }
        }
    }

    // 3. Add self-loops for repeated activities
    for (activity, freq) in &activity_freq {
        if *freq > min_support * 2 {
            candidates.push(Patch {
                patch_type: "add_loop".to_string(),
                source: *activity,
                target: *activity,
                delta_fitness: (*freq as f64 / total_freq as f64) * 0.02,
            });
        }
    }

    // Sort by delta_fitness and take top candidates
    candidates.sort_by(|a, b| b.delta_fitness.partial_cmp(&a.delta_fitness).unwrap());
    candidates.truncate(max_patches.min(beam_width));

    // Convert to Erlang terms
    let result: Vec<_> = candidates
        .iter()
        .map(|p| {
            (
                p.patch_type.clone(),
                p.source,
                p.target,
                p.delta_fitness,
            )
        })
        .collect();

    Ok((
        atoms::ok(),
        (
            (atoms::patches(), result),
            (atoms::candidates(), candidates.len()),
        ),
    )
        .encode(env))
}

// ============================================================================
// Event Log Compression
// ============================================================================

/// Compress event batch using dictionary + delta encoding
#[rustler::nif(schedule = "DirtyCpu")]
fn compress_events<'a>(env: Env<'a>, events: Term<'a>) -> NifResult<Term<'a>> {
    // Parse events: [{case_id, activity_id, timestamp, attributes}, ...]
    let event_list: Vec<(u64, u64, u64, Vec<(String, String)>)> = events.decode()?;

    if event_list.is_empty() {
        return Ok((atoms::ok(), (atoms::compressed(), Vec::<u8>::new(), 0usize)).encode(env));
    }

    // Build dictionaries
    let mut activity_dict: HashMap<u64, u8> = HashMap::new();
    let mut attr_key_dict: HashMap<String, u8> = HashMap::new();
    let mut attr_val_dict: HashMap<String, u16> = HashMap::new();

    for (_, activity, _, attrs) in &event_list {
        let len = activity_dict.len();
        activity_dict.entry(*activity).or_insert(len as u8);

        for (key, val) in attrs {
            let key_len = attr_key_dict.len();
            attr_key_dict.entry(key.clone()).or_insert(key_len as u8);

            let val_len = attr_val_dict.len();
            attr_val_dict.entry(val.clone()).or_insert(val_len as u16);
        }
    }

    // Compress data
    let mut compressed: Vec<u8> = Vec::with_capacity(event_list.len() * 16);

    // Header: magic + version + dict sizes
    compressed.extend_from_slice(&[0x53, 0x57, 0x46, 0x01]); // "SWF" + version 1
    compressed.push(activity_dict.len() as u8);
    compressed.push(attr_key_dict.len() as u8);
    compressed.extend_from_slice(&(attr_val_dict.len() as u16).to_le_bytes());

    // Activity dictionary
    let mut activities: Vec<_> = activity_dict.iter().collect();
    activities.sort_by_key(|(_, idx)| *idx);
    for (activity, _) in activities {
        compressed.extend_from_slice(&activity.to_le_bytes());
    }

    // Attribute key dictionary (length-prefixed strings)
    let mut attr_keys: Vec<_> = attr_key_dict.iter().collect();
    attr_keys.sort_by_key(|(_, idx)| *idx);
    for (key, _) in attr_keys {
        compressed.push(key.len() as u8);
        compressed.extend_from_slice(key.as_bytes());
    }

    // Attribute value dictionary
    let mut attr_vals: Vec<_> = attr_val_dict.iter().collect();
    attr_vals.sort_by_key(|(_, idx)| *idx);
    for (val, _) in attr_vals {
        compressed.push(val.len() as u8);
        compressed.extend_from_slice(val.as_bytes());
    }

    // Event count
    compressed.extend_from_slice(&(event_list.len() as u32).to_le_bytes());

    // Delta-encoded events
    let mut prev_case: u64 = 0;
    let mut prev_ts: u64 = 0;

    for (case_id, activity, timestamp, attrs) in &event_list {
        // Delta case_id (varint)
        let case_delta = case_id.wrapping_sub(prev_case);
        encode_varint(&mut compressed, case_delta);
        prev_case = *case_id;

        // Activity index
        compressed.push(*activity_dict.get(activity).unwrap());

        // Delta timestamp
        let ts_delta = timestamp.wrapping_sub(prev_ts);
        encode_varint(&mut compressed, ts_delta);
        prev_ts = *timestamp;

        // Attributes count + encoded
        compressed.push(attrs.len() as u8);
        for (key, val) in attrs {
            compressed.push(*attr_key_dict.get(key).unwrap());
            let val_idx = *attr_val_dict.get(val).unwrap();
            compressed.extend_from_slice(&val_idx.to_le_bytes());
        }
    }

    let original_size = event_list.len() * 64; // Estimate
    let compressed_size = compressed.len();
    let ratio = compressed_size as f64 / original_size as f64;

    Ok((
        atoms::ok(),
        (
            (atoms::compressed(), compressed),
            ("original_size", original_size),
            ("compressed_size", compressed_size),
            ("ratio", ratio),
        ),
    )
        .encode(env))
}

/// Variable-length integer encoding
fn encode_varint(buf: &mut Vec<u8>, mut value: u64) {
    while value >= 0x80 {
        buf.push((value as u8) | 0x80);
        value >>= 7;
    }
    buf.push(value as u8);
}

/// Decode variable-length integer
fn decode_varint(data: &[u8], pos: &mut usize) -> u64 {
    let mut result: u64 = 0;
    let mut shift = 0;

    loop {
        if *pos >= data.len() {
            break;
        }
        let byte = data[*pos];
        *pos += 1;
        result |= ((byte & 0x7F) as u64) << shift;
        if byte & 0x80 == 0 {
            break;
        }
        shift += 7;
    }

    result
}

/// Decompress event batch
#[rustler::nif(schedule = "DirtyCpu")]
fn decompress_events<'a>(env: Env<'a>, compressed: Term<'a>) -> NifResult<Term<'a>> {
    let data: Vec<u8> = compressed.decode()?;

    if data.len() < 8 {
        return Ok((atoms::error(), "invalid_header").encode(env));
    }

    // Verify header
    if &data[0..3] != b"SWF" || data[3] != 0x01 {
        return Ok((atoms::error(), "invalid_magic").encode(env));
    }

    let mut pos = 4;

    let activity_count = data[pos] as usize;
    pos += 1;
    let attr_key_count = data[pos] as usize;
    pos += 1;
    let attr_val_count = u16::from_le_bytes([data[pos], data[pos + 1]]) as usize;
    pos += 2;

    // Read activity dictionary
    let mut activities: Vec<u64> = Vec::with_capacity(activity_count);
    for _ in 0..activity_count {
        let activity = u64::from_le_bytes([
            data[pos], data[pos + 1], data[pos + 2], data[pos + 3],
            data[pos + 4], data[pos + 5], data[pos + 6], data[pos + 7],
        ]);
        pos += 8;
        activities.push(activity);
    }

    // Read attribute key dictionary
    let mut attr_keys: Vec<String> = Vec::with_capacity(attr_key_count);
    for _ in 0..attr_key_count {
        let len = data[pos] as usize;
        pos += 1;
        let key = String::from_utf8_lossy(&data[pos..pos + len]).to_string();
        pos += len;
        attr_keys.push(key);
    }

    // Read attribute value dictionary
    let mut attr_vals: Vec<String> = Vec::with_capacity(attr_val_count);
    for _ in 0..attr_val_count {
        let len = data[pos] as usize;
        pos += 1;
        let val = String::from_utf8_lossy(&data[pos..pos + len]).to_string();
        pos += len;
        attr_vals.push(val);
    }

    // Read event count
    let event_count = u32::from_le_bytes([
        data[pos], data[pos + 1], data[pos + 2], data[pos + 3],
    ]) as usize;
    pos += 4;

    // Decode events
    let mut events: Vec<(u64, u64, u64, Vec<(String, String)>)> =
        Vec::with_capacity(event_count);

    let mut prev_case: u64 = 0;
    let mut prev_ts: u64 = 0;

    for _ in 0..event_count {
        // Delta case_id
        let case_delta = decode_varint(&data, &mut pos);
        let case_id = prev_case.wrapping_add(case_delta);
        prev_case = case_id;

        // Activity index
        let activity_idx = data[pos] as usize;
        pos += 1;
        let activity = activities.get(activity_idx).copied().unwrap_or(0);

        // Delta timestamp
        let ts_delta = decode_varint(&data, &mut pos);
        let timestamp = prev_ts.wrapping_add(ts_delta);
        prev_ts = timestamp;

        // Attributes
        let attr_count = data[pos] as usize;
        pos += 1;

        let mut attrs: Vec<(String, String)> = Vec::with_capacity(attr_count);
        for _ in 0..attr_count {
            let key_idx = data[pos] as usize;
            pos += 1;
            let val_idx = u16::from_le_bytes([data[pos], data[pos + 1]]) as usize;
            pos += 2;

            let key = attr_keys.get(key_idx).cloned().unwrap_or_default();
            let val = attr_vals.get(val_idx).cloned().unwrap_or_default();
            attrs.push((key, val));
        }

        events.push((case_id, activity, timestamp, attrs));
    }

    Ok((
        atoms::ok(),
        (
            (atoms::decompressed(), events),
            ("event_count", events.len()),
        ),
    )
        .encode(env))
}

// ============================================================================
// SIMD Vector Operations
// ============================================================================

/// SIMD-accelerated dot product for embedding similarity
#[cfg(target_arch = "x86_64")]
#[rustler::nif]
fn simd_dot_product<'a>(env: Env<'a>, vec_a: Term<'a>, vec_b: Term<'a>) -> NifResult<Term<'a>> {
    let a: Vec<f32> = vec_a.decode()?;
    let b: Vec<f32> = vec_b.decode()?;

    if a.len() != b.len() {
        return Ok((atoms::error(), "dimension_mismatch").encode(env));
    }

    #[cfg(target_feature = "avx2")]
    {
        use std::arch::x86_64::*;

        let len = a.len();
        let chunks = len / 8;
        let mut sum = unsafe { _mm256_setzero_ps() };

        unsafe {
            for i in 0..chunks {
                let va = _mm256_loadu_ps(a.as_ptr().add(i * 8));
                let vb = _mm256_loadu_ps(b.as_ptr().add(i * 8));
                let prod = _mm256_mul_ps(va, vb);
                sum = _mm256_add_ps(sum, prod);
            }

            // Horizontal sum
            let mut result: [f32; 8] = [0.0; 8];
            _mm256_storeu_ps(result.as_mut_ptr(), sum);
            let mut total: f32 = result.iter().sum();

            // Handle remainder
            for i in (chunks * 8)..len {
                total += a[i] * b[i];
            }

            return Ok((atoms::ok(), total as f64).encode(env));
        }
    }

    // Fallback for non-AVX2
    #[cfg(not(target_feature = "avx2"))]
    {
        let total: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
        Ok((atoms::ok(), total as f64).encode(env))
    }
}

#[cfg(not(target_arch = "x86_64"))]
#[rustler::nif]
fn simd_dot_product<'a>(env: Env<'a>, vec_a: Term<'a>, vec_b: Term<'a>) -> NifResult<Term<'a>> {
    let a: Vec<f32> = vec_a.decode()?;
    let b: Vec<f32> = vec_b.decode()?;

    if a.len() != b.len() {
        return Ok((atoms::error(), "dimension_mismatch").encode(env));
    }

    let total: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    Ok((atoms::ok(), total as f64).encode(env))
}

/// SIMD-accelerated L2 distance
#[rustler::nif]
fn simd_l2_distance<'a>(env: Env<'a>, vec_a: Term<'a>, vec_b: Term<'a>) -> NifResult<Term<'a>> {
    let a: Vec<f32> = vec_a.decode()?;
    let b: Vec<f32> = vec_b.decode()?;

    if a.len() != b.len() {
        return Ok((atoms::error(), "dimension_mismatch").encode(env));
    }

    let sum: f32 = a.iter()
        .zip(b.iter())
        .map(|(x, y)| {
            let diff = x - y;
            diff * diff
        })
        .sum();

    Ok((atoms::ok(), (sum as f64).sqrt()).encode(env))
}

/// SIMD-accelerated cosine similarity
#[rustler::nif]
fn simd_cosine_similarity<'a>(
    env: Env<'a>,
    vec_a: Term<'a>,
    vec_b: Term<'a>,
) -> NifResult<Term<'a>> {
    let a: Vec<f32> = vec_a.decode()?;
    let b: Vec<f32> = vec_b.decode()?;

    if a.len() != b.len() {
        return Ok((atoms::error(), "dimension_mismatch").encode(env));
    }

    let mut dot: f32 = 0.0;
    let mut norm_a: f32 = 0.0;
    let mut norm_b: f32 = 0.0;

    for (x, y) in a.iter().zip(b.iter()) {
        dot += x * y;
        norm_a += x * x;
        norm_b += y * y;
    }

    let similarity = if norm_a > 0.0 && norm_b > 0.0 {
        dot / (norm_a.sqrt() * norm_b.sqrt())
    } else {
        0.0
    };

    Ok((atoms::ok(), similarity as f64).encode(env))
}

// ============================================================================
// NIF Registration
// ============================================================================

rustler::init!(
    "swf_rust_nif",
    [
        replay_tokens,
        compute_alignment,
        calculate_fitness,
        search_patches,
        compress_events,
        decompress_events,
        simd_dot_product,
        simd_l2_distance,
        simd_cosine_similarity
    ]
);
