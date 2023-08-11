import { LoadResponse } from "./LoadResponse";
import { verbose } from "./verbose";
import got from "got";

export class GotToResponse implements LoadResponse {
    private parsedJson: unknown;

    readonly status: number;
    constructor(public readonly url: string, private readonly response: got.Response<string>) {
        this.status = response.statusCode;
        verbose("Got response", url, response.statusCode);
    }

    text(): Promise<string> {
        return Promise.resolve(this.response.body);
    }

    json(): Promise<unknown> {
        if (this.parsedJson === undefined) {
            this.parsedJson = JSON.parse(this.response.body);
        }
        return Promise.resolve(this.parsedJson);
    }
}
