%%%-------------------------------------------------------------------
%%% @doc A2A Projection - "One Kernel, Two Faces"
%%%
%%% Maps A2A protocol to Case reality. A2A is just a face over the
%%% unified Case kernel. Every A2A Task IS a Case, every A2A Message
%%% becomes a Case signal, every A2A Artifact is a Case artifact.
%%%
%%% Architecture:
%%% - Task ID = Case ID (same thing)
%%% - SendMessageRequest → signal(Name, Payload)
%%% - Task state = projection of Case status
%%% - Artifacts = Case artifacts with PQC signatures
%%% - Subscriptions = pg subscription to Case events
%%%
%%% State Mapping:
%%% - submitted → TASK_STATE_SUBMITTED
%%% - running → TASK_STATE_WORKING
