import { LoadResponse } from "./LoadResponse";

export interface BuildResult {
    response: LoadResponse;
    info: Record<string, unknown>;
}
